module Library.Interface
    ( createSemesterInformation
    , createStudentInformation
    )where

import Control.Applicative
import Graphics.UI.WX
import qualified Database.Relations.Student as Student
import Database.Relations.Student
import qualified Database.Relations.Lecture as Lecture
import Database.Relations.Lecture


type Semester = String
createSemesterInformation :: Semester -> Layout
createSemesterInformation s = boxed "学期" $ grid 5 5 [[label "今学期:", label s]]

createStudentInformation :: Student -> Layout
createStudentInformation student = boxed "学生" $ grid 5 5 $ zipWith (\name value -> [label name, value]) atributeNames atributeValues
    where
        atributeNames = ["学生証番号:", "氏名:", "学部:", "学科:", "セメスター:"]
        atributeValues = map label $ map (\f -> f student) [show . studentNumber, Student.name, faculty, department, semester]
{-
    f: Frame
    wl: 各曜日の各時限の講義のリスト
    履修登録のレイアウトを生成する。
-}
type OneWeekLecture = [[Lecture]] -- [["オペレーティングシステム", "心理学の世界"], ["微分積分1", "線形代数1"], ...]
createCourseRegistration :: Frame () -> OneWeekLecture -> IO (Layout)
createCourseRegistration f wl =  do
    choices <- sequence $ map (\l -> fmap widget (choice f [items := ("--" : (map Lecture.name l))])) wl
    -- 各曜日のx限目の授業
    let takeSix = take 6
        choices_1 = (period !! 0) : (takeSix choices)
        choices_2 = (period !! 1) : (takeSix $ drop 6 choices)
        choices_3 = (period !! 2) : (takeSix $ drop 12 choices)
        choices_4 = (period !! 3) : (takeSix $ drop 18 choices)
        choices_5 = (period !! 4) : (takeSix $ drop 24 choices)
    return $ boxed "履修" (grid 5 5 [weeksLabel, choices_1, choices_2, choices_3, choices_4, choices_5])
    where
        period = map (label . show) [1, 2, 3, 4, 5]
        weeksLabel = map label ["", "月", "火", "水", "木", "金", "土"]


type Specilize = Int
type Common = Int
createRecord :: Frame () -> (Common, Specilize) -> IO (Layout)
createRecord f (c, s) = do
    commonValue <- fmap widget $ staticText f [text := show c]
    specilizeValue <- fmap widget $ staticText f [text := show s]
    return $ boxed "成績" $ row 10 [row 5 [commonLabel, commonValue], row 5 [specilizeLabel, specilizeValue]]
    where
        commonLabel = label "共通:"
        specilizeLabel = label "専門:"
