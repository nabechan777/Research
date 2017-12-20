module Library.Interface where

import Graphics.UI.WX
import Database.Relations.Student

type Period = String
type EachLecture = [[String]] -- [["オペレーティングシステム", "心理学の世界"], ["微分積分1", "線形代数1"], ...]
type OneWeekLecture = [[[String]]]

run :: IO ()
run = start $ do
    f <- frame [text := "Research"]
    let studentData = Student 1 12345 "山田太郎" "コンピュータ理工" "コンピュータサイエンス" "4セメ"
    semesterInformation <- createSemesterInformation f "秋"
    studentInformation <- createStudentInformation f studentData
    registration <- createCourseRegistration f createDummyData
    record <- createRecord f (20, 80)
    set f [layout := margin 10 $ column 10 [row 5 [semesterInformation, studentInformation], registration, record]]
    return ()
    where
        createDummyData :: OneWeekLecture
        createDummyData = replicate 5 $ replicate 6 ["Dummy1", "Dummy2", "Dummy3"]


type Semester = String
createSemesterInformation :: Frame () -> Semester -> IO (Layout)
createSemesterInformation f s = return $ boxed "学期" $ grid 5 5 [[label "今学期:", label s]]

createStudentInformation :: Frame () -> Student -> IO (Layout)
createStudentInformation f student = return $ boxed "学生" $ grid 5 5 $ zipWith (\name value -> [label name, value]) atributeNames atributeValues
    where
        atributeNames = ["学生証番号:", "氏名:", "学部:", "学科:", "セメスター:"]
        atributeValues = map label $ map (\f -> f student) [show . studentNumber, name, faculty, department, semester]
{-
    f: Frame
    wl: 各曜日の各時限の講義のリスト
    履修登録のレイアウトを生成する。
-}
createCourseRegistration :: Frame () -> OneWeekLecture -> IO (Layout)
createCourseRegistration f wl = do
    rows <- sequence $ zipWith createCourseRegistration' ["1", "2", "3", "4", "5"] wl
    return $ boxed "履修" (grid 5 5 (weeksLabel : rows))
    where
        weeksLabel = map label ["", "月", "火", "水", "木", "金", "土"]
        createCourseRegistration' :: Period -> EachLecture -> IO ([Layout])
        createCourseRegistration' p el = do
            lectures <- sequence $ map (\l -> choice f [items := l]) el
            return $ label p : (map widget lectures)

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
