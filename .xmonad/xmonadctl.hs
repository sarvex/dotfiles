import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Environment
import System.IO
import Data.Char

main :: IO ()
main = parse True "XMONAD_COMMAND" =<< getArgs

parse :: Bool -> String -> [String] -> IO ()
parse input addr args = case args of
        ["--"] | input -> repl addr
               | otherwise -> return ()
        ("--":xs) -> sendAll addr xs
        ("-a":a:xs) -> parse input a xs
        ("-h":_) -> showHelp
        ("--help":_) -> showHelp
        ("-?":_) -> showHelp
        (a@('-':_):_) -> hPutStrLn stderr ("Unknown option " ++ a)

        (x:xs) -> sendCommand addr x >> parse False addr xs
        [] | input -> repl addr
           | otherwise -> return ()


repl :: String -> IO ()
repl addr = do e <- isEOF
               case e of
                True -> return ()
                False -> do l <- getLine
                            sendCommand addr l
                            repl addr

sendAll :: String -> [String] -> IO ()
sendAll addr ss = foldr (\a b -> sendCommand addr a >> b) (return ()) ss

sendCommand :: String -> String -> IO ()
sendCommand addr s = do
  d   <- openDisplay ""
  rw  <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d s False
  allocaXEvent $ \e -> do
                  setEventType e clientMessage
                  setClientMessageEvent e rw a 32 m currentTime
                  sendEvent d rw False structureNotifyMask e
                  sync d False

showHelp :: IO ()
showHelp = do pn <- getProgName
              putStrLn ("Send commands to a running instance of xmonad. xmonad.hs must be configured with XMonad.Hooks.ServerMode to work.\n-a atomname can be used at any point in the command line arguments to change which atom it is sending on.\nIf sent with no arguments or only -a atom arguments, it will read commands from stdin.\nEx:\n" ++ pn ++ " cmd1 cmd2\n" ++ pn ++ " -a XMONAD_COMMAND cmd1 cmd2 cmd3 -a XMONAD_PRINT hello world\n" ++ pn ++ " -a XMONAD_PRINT # will read data from stdin.\nThe atom defaults to XMONAD_COMMAND.")


 -- 1 - view"<action=xdotool key super+1>dev</action>"
 -- 2 - shift"<action=xdotool key super+1>dev</action>"
 -- 3 - view"<action=xdotool key super+2>www</action>"
 -- 4 - shift"<action=xdotool key super+2>www</action>"
 -- 5 - view"<action=xdotool key super+3>sys</action>"
 -- 6 - shift"<action=xdotool key super+3>sys</action>"
 -- 7 - view"<action=xdotool key super+4>doc</action>"
 -- 8 - shift"<action=xdotool key super+4>doc</action>"
 -- 9 - view"<action=xdotool key super+5>vbox</action>"
 -- 10 - shift"<action=xdotool key super+5>vbox</action>"
 -- 11 - view"<action=xdotool key super+6>chat</action>"
 -- 12 - shift"<action=xdotool key super+6>chat</action>"
 -- 13 - view"<action=xdotool key super+7>mus</action>"
 -- 14 - shift"<action=xdotool key super+7>mus</action>"
 -- 15 - view"<action=xdotool key super+8>vid</action>"
 -- 16 - shift"<action=xdotool key super+8>vid</action>"
 -- 17 - view"<action=xdotool key super+9>gfx</action>"
 -- 18 - shift"<action=xdotool key super+9>gfx</action>"
 -- 19 - screen0
 -- 20 - screen-to-0
 -- 21 - screen1
 -- 22 - screen-to-1
 -- 23 - shrink
 -- 24 - expand
 -- 25 - next-layout
 -- 26 - default-layout
 -- 27 - restart-wm
 -- 28 - restart-wm-no-resume
 -- 29 - xterm
 -- 30 - run
 -- 31 - kill
 -- 32 - refresh
 -- 33 - focus-up
 -- 34 - focus-down
 -- 35 - swap-up
 -- 36 - swap-down
 -- 37 - swap-master
 -- 38 - sink
 -- 39 - quit-wm
