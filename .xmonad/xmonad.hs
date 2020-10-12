{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import Graphics.X11.ExtraTypes
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnOnce)


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and
-- by certain contrib modules.

myTerminal :: String
myTerminal = "urxvt"

-- The command to lock the screen or show the screensaver.
myScreensaver :: String
myScreensaver = "slock"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot :: String
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot :: String
myScreenshot = "screenshot"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher :: String
myLauncher = "rofi -show run"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.

myWorkspaces :: [String]
myWorkspaces = clickable workspaceLabels
 where
  clickable l =

    [ "<action=xdotool key super+"
        ++ show i
        ++ " button=1>"
        ++ ws
        ++ "</action>"
    | (i, ws) <- zip ([1 .. 9] ++ [0 :: Int]) l
    ]
  workspaceLabels = zipWith makeLabel [1 .. 10 :: Int] icons
  makeLabel index (fontIndex, icon) = show index ++ ":<fn=" ++ show fontIndex ++ ">" ++ icon : "</fn>"
  icons =
    [ (2, '\xf269')
    , (1, '\xf120')
    , (1, '\xf121')
    , (1, '\xf03d')
    , (1, '\xf128')
    , (1, '\xf128')
    , (1, '\xf128')
    , (1, '\xf128')
    , (2, '\xf1b6')
    , (2, '\xf1bc')
    ]

-----------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook :: ManageHook
myManageHook = manageSpawn <+> composeAll
  [ isDialog --> placeHook (fixed (0.5, 0.5))
  , className =? "Spotify" --> doShift (myWorkspaces !! 9)
  , resource =? "desktop_window" --> doIgnore
  , className =? "Gimp" --> doFloat
  , className =? "Oblogout" --> doFloat
  , className =? "vlc" --> doShift (myWorkspaces !! 8)
  , className =? "Steam" --> doShift (myWorkspaces !! 8)
  , className =? "Gxmessage" --> doFloat
  , className =? "XClock" --> placeHook (underMouse (0.5, -0.3)) <+> doFloat
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  ]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout = avoidStruts (mySpacing (Tall 1 (3 / 100) (1 / 2)))
  ||| avoidStruts (fullscreenFull Full)
  ||| fullscreenFull Full
 where
  mySpacing = spacingRaw False (Border 20 20 20 20) True (Border 0 0 0 0) False .
    spacingRaw True (Border 20 20 20 20) False (Border 20 20 20 20) True

------------------------------------------------------------------------
-- Colors and borders

myNormalBorderColor :: String
myNormalBorderColor = "#FAFAFA"

myFocusedBorderColor :: String
myFocusedBorderColor = myNormalBorderColor

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#Af745f"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 0


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask :: KeyMask
myModMask = mod4Mask

mySink :: String
mySink = "alsa_output.pci-0000_00_1f.3.analog-stereo"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { modMask } =
  M.fromList
    $ [

    -- Start the terminal specified by myTerminal variable
        ((modMask, xK_Return), spawn $ XMonad.terminal conf)
      ,

    -- Lock the screen using command specified by myScreensaver
        ((modMask .|. controlMask, xK_l), spawn myScreensaver)
      ,

    -- Spawn the launcher using command specified by myLauncher
        ((modMask, xK_space)          , spawn myLauncher)
      ,

    -- Take a selective screenshot using the command specified by mySelectScreenshot
        ((modMask .|. shiftMask, xK_p), spawn mySelectScreenshot)
      ,

    -- Take a full screenshot using the command specified by myScreenshot
        ((modMask .|. controlMask .|. shiftMask, xK_p), spawn myScreenshot)
      ,

    -- Mute volume
        ( (0, xF86XK_AudioMute)
        , spawn $ "pactl set-sink-mute " ++ mySink ++ " toggle"
        )
      ,

    -- Decrease volume
        ( (0, xF86XK_AudioLowerVolume)
        , spawn
        $  "pactl set-sink-mute "
        ++ mySink
        ++ " false; pactl set-sink-volume "
        ++ mySink
        ++ " -5%"
        )
      ,

    -- Increase volume
        ( (0, xF86XK_AudioRaiseVolume)
        , spawn
        $  "pactl set-sink-mute "
        ++ mySink
        ++ " false; pactl set-sink-volume "
        ++ mySink
        ++ " +5%"
        )
      ,

    -- Decrease brightness
        ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
      ,

    -- Increase brightness
        ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
      ,

    -- Audio previous
        ((0, xF86XK_AudioPrev), spawn "playerctl previous")
      ,

    -- Play/pause
        ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
      ,

    -- Audio next
        ((0, xF86XK_AudioNext), spawn "playerctl next")
      ,

    -- Move to the next open workspace
        ((modMask .|. shiftMask, xK_Tab), moveTo Next NonEmptyWS)
      ,

    -- Move to the previous open workspace
        ((modMask .|. shiftMask .|. mod1Mask, xK_Tab), moveTo Prev NonEmptyWS)
      ,

    -- Move to the next empty workspace
        ((modMask, xK_e), moveTo Next EmptyWS)
      , ((modMask .|. mod1Mask, xK_w), toggleHDMI)
      ,

    -- Bindings for xmonad-session
    -- ((modm, xK_s), toggleSaveState),

    -- ((modm .|. shiftMask, xK_s), launchDocuments),

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

    -- Close focused window
        ((modMask .|. shiftMask, xK_w), kill)
      ,

    -- Cycle through the available layout algorithms
        ((modMask, xK_Right), sendMessage NextLayout)
      ,

    --  Reset the layouts on the current workspace to default
        ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
      ,

    -- Resize viewed windows to the correct size
        ((modMask, xK_n)      , refresh)
      ,

    -- Move focus to the next window
        ((modMask, xK_Tab), windows W.focusDown)
      ,

    -- Move focus to the next window
        ((modMask, xK_j), windows W.focusDown)
      ,

    -- Move focus to the previous window
        ((modMask, xK_k), windows W.focusUp)
      ,

    -- Move focus to the master window
        ((modMask, xK_m), windows W.focusMaster)
      ,

    -- Swap the focused window and the master window
        ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
      ,

    -- Swap the focused window with the next window
        ((modMask .|. shiftMask, xK_j), windows W.swapDown)
      ,

    -- Swap the focused window with the previous window
        ((modMask .|. shiftMask, xK_k), windows W.swapUp)
      ,

    -- Shrink the master area
        ((modMask, xK_h), sendMessage Shrink)
      ,

    -- Expand the master area
        ((modMask, xK_l), sendMessage Expand)
      ,

    -- Push window back into tiling
        ((modMask, xK_t), withFocused $ windows . W.sink)
      ,

    -- Increment the number of windows in the master area
        ((modMask, xK_comma), sendMessage (IncMasterN 1))
      ,

    -- Decrement the number of windows in the master area
        ((modMask, xK_period), sendMessage (IncMasterN (-1)))
      ,

    -- Quit xmonad
        ((modMask .|. shiftMask, xK_x), io exitSuccess)
      ,

    -- Restart xmonad
        ((modMask, xK_x), restart "xmonad" True)
      ]
    ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

toggleHDMI :: X ()
toggleHDMI = do
  (count :: Int) <- countScreens
  spawn $ "echo " ++ show count ++ " >> ~/test.txt"
  if count > 1
    then spawn "xrandr --output HDMI1 --off"
    else spawn "sleep 0.3; xrandr --output HDMI1 --auto --right-of eDP1"

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { modMask } = M.fromList
  [
    -- Set the window to floating mode and move by dragging
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
  ,

    -- Raise the window to the top of the stack
    ((modMask, button2), \w -> focus w >> windows W.swapMaster)
  ,

    -- Set the window to floating mode and resize by dragging
    ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
  ]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook :: X ()
myStartupHook =
  spawn "xsetroot -cursor_name left_ptr"
    <+> spawn "xrandr --output HDMI1 --off"
    <+> spawn "xrandr --output HDMI1 --auto --right-of eDP1"
    <+> setWMName "LG3D"
    <+> spawn "xssproxy"
    <+> spawn "xprop -root -f focus 8s -set focus \"What's your new focus?\""
    <+> spawn "xprop -root -f pom-timer 8s -set pom-timer \"<action=\\`pom start\\` button=1>Start a pomodoro</action>\""
    <+> spawnOnOnce (myWorkspaces !! 0) "firefox"

-----------------------------------------------------------------------
-- Log hook

myLogHook :: Handle -> X ()
myLogHook xmproc = do
  fadeInactiveLogHook 0.5
  dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = const ""
    , ppLayout  = const ""
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep     = "   "
    }

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -d ~/.xmonad/xmobar.hs"
  xmonad . docks . E.ewmh $ defaults { logHook = myLogHook xmproc }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.

defaults = def
  {
    -- simple stuff
    terminal      = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , borderWidth   = myBorderWidth
  , modMask       = myModMask
  , workspaces    = myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor

    -- key bindings
  , keys          = myKeys
  , mouseBindings = myMouseBindings

    -- hooks, layouts
  , layoutHook    = myLayout
  , manageHook    = myManageHook
  , startupHook   = myStartupHook
  , handleEventHook = E.fullscreenEventHook
  }
