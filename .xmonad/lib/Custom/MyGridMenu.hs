module Custom.MyGridMenu where

  -- imports
import XMonad
import XMonad.Actions.GridSelect
import Custom.MyVariables

------------------------------------------------------------------------
-- GRIDSELECT
------------------------------------------------------------------------
-- GridSelect displays items (programs, open windows, etc.) in a 2D grid
-- and lets the user select from it with the cursor/hjkl keys or the mouse.
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }


myAppsFave :: [(String, String, String)]
myAppsFave = [ ("Audacity", "audacity", "Graphical cross-platform audio eidtor")
                 , ("Deadbeef", "deadbeef", "Lightweight GUI audio player")
                 , ("Emacs", "emacs", "Much more than a text editor")
                 , ("Firefox", "firefox", "The famous open source web browser")
                 , ("Geany", "geany", "A nice text editor")
                 , ("Geary", "geary", "Email client that is attractive")
                 , ("Gimp", "gimp", "Open source alternative to Photoshop")
                 , ("Kdenlive", "kdenlive", "A great open source video editor")
                 , ("LibreOffice Impress", "loimpress", "For making presentations")
                 , ("LibreOffice Writer", "lowriter", "A fully featured word processor")
                 , ("OBS", "obs", "Open broadcaster software")
                 , ("PCManFM", "pcmanfm", "Lightweight graphical file manager")
                 ]
 
-- Let's take myAppsFave, myBookmarks and myConfigs and take only
-- the first two values from those 3-tuples (for GridSelect).
myAppGrid :: [(String, String)]
myAppGrid = [ (a,b) | (a,b,c) <- xs]
  where xs = myAppsFave

