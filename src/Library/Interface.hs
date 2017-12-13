module Library.Interface where

import Graphics.UI.WX

type OneDayLecture = [[String]] -- [["オペレーティングシステム", "心理学の世界"], ["微分積分1", "線形代数1"], ...]
type OneWeekLecture = [[[String]]]

run :: IO ()
run = start $ do
    f <- createFrame
    choices <- createChoices f createDummyData
    -- c <- choice f [items := ["A", "B", "C"]]
    -- mapM_ (itemAppend c) ["A", "B", "C"]
    set f [layout := choices]
    return ()
    where
        createDummyData :: OneWeekLecture
        createDummyData = replicate 7 $ replicate 5 ["Dummy1", "Dummy2", "Dummy3"]

createFrame :: IO (Frame ())
createFrame = do
    let windowSize = sz 1000 700 :: Size
        windowPoint = point 100 100 :: Point
    f <- frame [text := "Research", clientSize := windowSize, resizeable := True]
    return f

createChoices :: Frame () -> OneWeekLecture -> IO (Layout)
createChoices f wl = do
    columns <- sequence $ map createChoices' wl
    return $ row 10 columns
    where
        createChoices' :: OneDayLecture -> IO (Layout)
        createChoices' ol = do
            choices <- sequence $ map (\l -> choice f [items := l]) ol
            let widgets = map widget choices
                colums = column 10 widgets
            return colums
