import XMonad
import Data.Monoid
import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = def
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , keys              = myKeys
    , layoutHook        = myLayout
    --, manageHook        = myManageHook <+> doCenterFloat <+> manageHook def <+> manageDocks
    --, manageHook        = doCenterFloat <+> myManageHook
    , manageHook        = myManageHook <+> manageSpawn
    , handleEventHook   = myEventHook
    , startupHook       = myStartupHook
    , logHook           = myLogHook
    }

myTerminal      = "xterm"

myModMask       = mod4Mask

myBorderWidth   = 4

myNormalBorderColor     = "#005f00" --"#000000"

myFocusedBorderColor    = "#dc322f" --"#FFFFFF" "#dc322f" "#005f00" "#ff0000" "#222200"

myWorkspaces = [ "main", "aux" ]
--myWorkspaces = [ "emacs", "www", "bt", "mus", "rss", "wts", "1", "2", "3"]
--myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]
--myWorkspaces = [ "Web", "Evernote", "Drafting", "Shell", "Mail", "Music", "IRC", "News", "Transmission", "Misc."]
--myWorkspaces = [ "Web", "Drafting", "Shell1", "Shell2", "Mail", "Music", "IRC", "News", "Misc."]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- github boylemic/configs
    -- launch a terminal
    [ ((myModMask,              xK_Return), spawnHere "xterm")

    -- launch dmenu
    --, ((myModMask,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((myModMask .|. shiftMask, xK_o     ), spawn "dmenu_run")

    -- launch gmrun
    --, ((myModMask .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((myModMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((myModMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((myModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- workspaces from connermcd
    , ((modMask,               xK_s     ), toggleWS)
    , ((modMask .|. shiftMask, xK_s     ), prevWS)
    , ((modMask,               xK_n     ), nextWS)
    , ((modMask,               xK_p     ), prevWS)
    , ((modMask .|. shiftMask, xK_p     ), shiftToPrev >> prevWS)
    , ((modMask .|. shiftMask, xK_n     ), shiftToNext >> nextWS)

    -- Resize viewed windows to the correct size
    , ((myModMask,               xK_r   ), refresh)

    -- Move focus to the next window
    , ((myModMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((myModMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((myModMask,               xK_k     ), windows W.focusUp  )

    -- Volume Control
    ,((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")

    -- Brightness Control
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")

    -- Move focus to the master window
    , ((myModMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((myModMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((myModMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((myModMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((myModMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((myModMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((myModMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((myModMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((myModMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((myModMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    --My Added Ones
    , (( myModMask, xK_f), safeSpawn "firefox" [])
    , (( myModMask, xK_w), spawn "qt.sh")
    , (( myModMask, xK_e), spawn "emacsclient-emacs-27-vcs -nc")
    , (( myModMask .|. shiftMask, xK_l ), spawnHere "xterm -e lawflash.sh")
    , (( myModMask .|. shiftMask, xK_w ), spawn "xterm -e w3m")
    , (( myModMask .|. shiftMask, xK_r), spawn "xterm -e ranger")
    , (( myModMask .|. shiftMask, xK_n ), spawn "xterm -e newsboat")
    , (( myModMask .|. shiftMask, xK_y), spawnHere "xterm -e mpsyt")
    , (( myModMask .|. shiftMask, xK_m), spawn "xterm -e mutt")
    , (( myModMask,               xK_d     ), withFocused (keysResizeWindow (-10,0) (1,1)))
    , (( myModMask,               xK_semicolon     ), withFocused (keysResizeWindow (10,0) (1,1)))
    , (( myModMask .|. shiftMask, xK_d     ), withFocused (keysResizeWindow (0,-10) (1,1)))
    , (( myModMask .|. shiftMask, xK_semicolon     ), withFocused (keysResizeWindow (0,50) (1%2,1)))
    , (( myModMask,               xK_a     ), withFocused (keysMoveWindowTo (1900,1000) (1,1)))
    , (( myModMask,               xK_Right     ), withFocused (keysMoveWindow (10,0) ))
    , (( myModMask,               xK_Down     ), withFocused (keysMoveWindow (0,10) ))
    , (( myModMask,               xK_Left     ), withFocused (keysMoveWindow (-10,0) ))
    , (( myModMask,               xK_Up     ), withFocused (keysMoveWindow (0,-10) ))
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myLayout = nobordersLayout ||| Mirror tiled ||| tiled ||| tiledR ||| StackTile 1 (3/100) (2/3) ||| simpleFloat
    --myLayout = mkToggle (single REFLECTX) $
    --           mkToggle (single REFLECTY) $
    --               (tiled ||| tiledR ||| Mirror tiled ||| Full)
                  where
                       -- default tiling algorithm partitions the screen into two panes
                       tiled = Tall nmaster delta ratio

                       -- reflected default tiling algorithm partitions the screen into two panes
                       tiledR = reflectHoriz $ Tall nmaster delta ratio

                       -- The default number of windows in the master pane
                       nmaster = 1

                       -- Default proportion of screen occupied by master pane
                       ratio = (1/2)

                       -- Percent of screen to increment by when resizing panes
                       delta = (3/100)

nobordersLayout = smartBorders $ Full

myManageHook = composeAll
     [ className =? "qutebrowser" --> doShift "1"
     , className =? "mpv" --> doFloat
     , className =? "feh" --> doFloat
     , className =? "emacs" --> doFloat
     ]

myEventHook = fadeWindowsEventHook {- ... -}

myStartupHook = do
    --myStartupHook = ewmhDesktopsStartup
    --myStartupHook :: X ()
    --ewmhDesktopsStartup
    --spawnOn "1" "qt.sh"
    spawnOn "main" "emacs-27-vcs --daemon"
    --spawnOn "9" "firefox"
    --spawnOn "emacs" "emacs-27-vcs"
    --spawnOn "bt" "xterm -e transmission-daemon"
    --spawnOn "rss" "xterm -e newsboat"
    --spawnOn "wts" "xterm -e 'watch ts"
    --spawnOn "2" "urxvt"
    --spawnOn "5" "mpv"
    --spawnOn "www" "xterm -e w3m -v"
    --spawnOn "Mail" "mutt"
    --spawnOn "Music" "vimpc"


myLogHook = fadeWindowsLogHook myFadeHook

myFadeHook = composeAll [opacity 1.00
                        , isUnfocused --> opacity 1.00
                        ]
