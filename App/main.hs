import Graphics.Gloss
import Bubbles
import System.Environment
import System.Exit

data SimMode = Small | Medium | Large | Huge deriving (Show, Eq)

getModeConfig :: SimMode -> ((Int, Int), Int, String)
getModeConfig Small  = ((100, 100), 1000, "Small (100x100)")
getModeConfig Medium = ((200, 200), 1000, "Medium (200x200)")
getModeConfig Large  = ((300, 300), 1500, "Large (300x300)")
getModeConfig Huge   = ((400, 400), 2000, "Huge (400x400)")

simMode :: String -> Maybe SimMode
simMode "1" = Just Small
simMode "2" = Just Medium
simMode "3" = Just Large
simMode "4" = Just Huge
simMode _   = Nothing

colorMode :: String -> Maybe ColorMode
colorMode "1" = Just Red
colorMode "2" = Just Green
colorMode "3" = Just Blue
colorMode "4" = Just White
colorMode "5" = Just Orange
colorMode "6" = Just Rainbow
colorMode _   = Nothing

getColorName :: ColorMode -> String
getColorName Red     = "Red"
getColorName Green   = "Green"
getColorName Blue    = "Cyan"
getColorName White   = "White"
getColorName Orange  = "Miranda"
getColorName Rainbow = "Rainbow"

main :: IO ()
main = do
  args <- getArgs
  (simMode, colorMode) <- case args of
    [simArg, colorArg] -> do
      case (simMode simArg, colorMode colorArg) of
        (Just sm, Just cm) -> return (sm, cm)
        _ -> do
          putStrLn "Invalid arguments. Usage: program <sim-mode> <color-mode>"
          putStrLn "Sim modes: 1=Small, 2=Medium, 3=Large, 4=Huge"
          putStrLn "Color modes: 1=Red, 2=Green, 3=Cyan, 4=White, 5=Miranda, 6=Rainbow"
          exitFailure
    [] -> do
      putStrLn "Choose simulation size:"
      putStrLn "1. Small (100x100) -> Smoothest"
      putStrLn "2. Medium (200x200)"
      putStrLn "3. Large (300x300)"
      putStrLn "4. Huge (400x400) -> ! Umm it barely runs !"
      putStr "Enter choice (1-4): "
      simChoice <- getLine
      simMode' <- case simMode simChoice of
        Just sm -> return sm
        Nothing -> do
          putStrLn "Invalid choice. Using Medium."
          return Medium
      
      putStrLn "\nChoose color mode:"
      putStrLn "1. Red"
      putStrLn "2. Green"
      putStrLn "3. Cyan"
      putStrLn "4. White"
      putStrLn "5. Miranda"
      putStrLn "6. Rainbow"
      putStr "Enter choice (1-6): "
      colorChoice <- getLine
      colorMode' <- case colorMode colorChoice of
        Just cm -> return cm
        Nothing -> do
          putStrLn "Invalid choice. Using Rainbow."
          return Rainbow
      
      return (simMode', colorMode')
    _ -> do
      putStrLn "Usage: program <sim-mode> <color-mode>"
      putStrLn "Or run without arguments for interactive mode"
      exitFailure
  
  let ((width, height), windowSize, modeName) = getModeConfig simMode
  let colorName = getColorName colorMode

  
  putStrLn $ "Starting " ++ modeName ++ " with " ++ colorName ++ " color mode"
  
  world <- initialWorld (width, height)
  
  play
    (InWindow ("BubbleWorld - " ++ modeName ++ " - " ++ colorName) (windowSize, windowSize) (10, 10))
    black
    60
    world
    (drawWorldWithColor colorMode)
    handleEvent
    (\dt -> update dt 0.15 60)