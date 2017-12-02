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
import XMonad.Layout.Tabbed 
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run (safeSpawn)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


myConfig = defaultConfig 
--myConfig = kdeConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , keys              = myKeys
    , layoutHook        = myLayout
    , manageHook        = myManageHook
    , handleEventHook   = myEventHook
    , startupHook       = myStartupHook
    , logHook           = myLogHook
    --, logHook           = fadeWindowsLogHook myFadeHook
    --, handleEventHook = fadeWindowsEventHook
    {- ... -}
}

--`additionalKeys` [ (( myModMask, xK_f), safeSpawn "firefox" [])
--   , (( myModMask, xK_p), spawn "dmenu_run")
--   , (( myModMask, xK_q), spawn "qt.sh")
--   , (( myModMask, xK_r), spawn "urxvt -e ranger")
--   , (( myModMask, xK_n), spawn "urxvt -e newsbeuter")
--   , (( myModMask, xK_y), spawn "urxvt -e mpsyt")
--   , (( myModMask, xK_m), spawn "urxvt -e mutt")
--   , (( myModMask,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1%2,1%2)))
--   , (( myModMask,               xK_s     ), withFocused (keysResizeWindow (10,10) (1%2,1%2)))
--   , (( myModMask .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
--   , (( myModMask .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
--   , (( myModMask,               xK_a     ), withFocused (keysMoveWindowTo (800,384) (1%2,1%2)))
--   , (( myModMask,               xK_Right     ), withFocused (keysMoveWindow (10,0) ))
--   , (( myModMask,               xK_Down     ), withFocused (keysMoveWindow (0,10) ))
--   , (( myModMask,               xK_Left     ), withFocused (keysMoveWindow (-10,0) ))
--   , (( myModMask,               xK_Up     ), withFocused (keysMoveWindow (0,-10) ))
--   ]
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	 -- github boylemic/configs
     -- launch a terminal
    [ ((myModMask,              xK_Return), spawn "urxvt")
 
    -- launch dmenu
    --, ((myModMask,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((myModMask,               xK_p     ), spawn "dmenu_run")
 
    -- launch gmrun
    --, ((myModMask .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window
    , ((myModMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((myModMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((myModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((myModMask,               xK_n     ), refresh)
 
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
    --, (( myModMask, xK_p), spawn "dmenu_run")
    , (( myModMask, xK_w), spawn "qt.sh")
    , (( myModMask .|. shiftMask, xK_w ), spawn "urxvt -e w3m")
    , (( myModMask .|. shiftMask, xK_r), spawn "urxvt -e ranger")
    , (( myModMask .|. shiftMask, xK_n ), spawn "urxvt -e newsbeuter")
    , (( myModMask .|. shiftMask, xK_y), spawn "urxvt -e mpsyt")
    , (( myModMask .|. shiftMask, xK_m), spawn "urxvt -e mutt")
    , (( myModMask,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1%2,1%2)))
    , (( myModMask,               xK_s     ), withFocused (keysResizeWindow (10,10) (1%2,1%2)))
    , (( myModMask .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
    , (( myModMask .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
    , (( myModMask,               xK_a     ), withFocused (keysMoveWindowTo (800,384) (1%2,1%2)))
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

myBorderWidth   = 1
--myFocusedBorderColor    = "#dc322f"
--myFocusedBorderColor    = "#005f00"
-- myFocusedBorderColor    = "#ff0000"
myFocusedBorderColor    = "#FFFFFF"
-- myFocusedBorderColor    = "#222200"
myNormalBorderColor     = "#000000"
myModMask       = mod4Mask
myTerminal      = "urxvt"
--myTerminal      = "konsole"
-- myWorkspaces = [ "Web", "Evernote", "Drafting", "Shell", "Mail", "Music", "IRC", "News", "Transmission", "Misc."]
-- myWorkspaces = [ "Web", "Drafting", "Shell1", "Shell2", "Mail", "Music", "IRC", "News", "Misc."]
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]

myManageHook = composeAll
     [ isFullscreen        --> doFullFloat
     , className =? "Alsamixer" --> doFloat
     , className =? "mpv" --> doFloat
     , className =? "feh" --> doFloat
     --, appName =? "ranger" --> doF W.swapDown
     , appName =? "ranger" --> doFloat
     , appName =? "qt.sh" --> doShift "1"
     --, insertPosition Below Newer
     , transience'
     ]

    <+> manageDocks <+> manageHook defaultConfig

myEventHook = fadeWindowsEventHook {- ... -}

--myFadeHook = composeAll [isUnfocused --> opacity 0.60
--                        ,                opacity 0.75
--                        ]

myFadeHook = composeAll [opacity 0.98
                        , isUnfocused --> opacity 0.95
                        ]

myLogHook = fadeWindowsLogHook myFadeHook
myLayout = nobordersLayout ||| Mirror tiled ||| tiled ||| tiledR ||| simpleFloat
--myLayout = mkToggle (single REFLECTX) $
--           mkToggle (single REFLECTY) $
--               (tiled ||| tiledR ||| Mirror tiled ||| Full)
                  where  
                       -- default tiling algorithm partitions the screen into two panes  
                       tiled = spacing 3 $ Tall nmaster delta ratio  
                    
                       -- reflected default tiling algorithm partitions the screen into two panes  
                       tiledR = spacing 3 $ reflectHoriz $ Tall nmaster delta ratio  
                    
                       -- The default number of windows in the master pane  
                       nmaster = 1  
                    
                       -- Default proportion of screen occupied by master pane  
                       ratio = 1/2  
                    
                       -- Percent of screen to increment by when resizing panes  
                       delta = 3/100

-- Define layout for specific workspaces
nobordersLayout = smartBorders $ Full

--myKeys = [ ((myModMask .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
--         , ((myModMask .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
--         ]
--      mkToggle (single REFLECTX) $
--      mkToggle (single REFLECTY) $
--        (tiled ||| tiledR ||| Mirror tiled ||| Full  )
--
--      , ((modm .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
--      , ((modm .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)


--myStartupHook = ewmhDesktopsStartup
--myStartupHook :: X ()
myStartupHook = do
    ewmhDesktopsStartup
--    spawnOn "1" "qt.sh"
--    spawnOn "2" "urxvt"
--    spawnOn "5" "mpv"
    --spawnOn "Shell1" "xterm"
    --spawnOn "Mail" "mutt"
    --spawnOn "Music" "vimpc"

-- toggle the status bar gap
--[
--((modMask,      xK_f    ), sendMessage ToggleStruts)
--((modMask,        xK_semicolon), windows W.shiftMaster)
--]

-- myLogHook = fadeInactiveLogHook fadeAmount
--     where fadeAmount = 0.30
-- 
