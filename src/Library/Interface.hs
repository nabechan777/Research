module Library.Interface
    ( createSemesterInformation
    , createStudentInformation
    , createLectureChoices
    , createRegistrationBotton
    , createRegistrationBoxed
    , createStaticTextPair
    , createRecordBoxed
    )where

import           Control.Applicative
import           Database.Relations.Grade   as G
import           Database.Relations.Lecture as L
import           Database.Relations.Student as S
import           Graphics.UI.WX


type Semester = String
createSemesterInformation :: Semester -> Layout
createSemesterInformation s = boxed "学期" $ grid 5 5 [[label "今学期:", label s]]

createStudentInformation :: Student -> Layout
createStudentInformation student = boxed "学生" $ grid 5 5 $ zipWith (\name value -> [label name, value]) atributeNames atributeValues
    where
        atributeNames = ["学生証番号:", "氏名:", "学部:", "学科:", "セメスター:"]
        atributeValues = map (label . (\f -> f student)) [show . studentNumber, S.name, faculty, department, semester]
{-
    f: Frame
    wl: 各曜日の各時限の講義のリスト
    履修登録のレイアウトを生成する。
-}
type OneWeekLecture = [[Lecture]] -- [["オペレーティングシステム", "心理学の世界"], ["微分積分1", "線形代数1"], ...]
createLectureChoices :: Frame () -> OneWeekLecture -> IO [Choice ()]
createLectureChoices f = mapM (\x -> choice f [items := "--" : map L.name x])

createRegistrationBotton :: Frame () -> IO (Button ())
createRegistrationBotton = (`button` [text := "登録", enabled := True])

createRegistrationBoxed :: [Choice ()] -> Button () -> Layout
createRegistrationBoxed xs b = boxed "履修" $ column 10 [lchoices, (floatRight . widget) b]
    where
        tmp1 :: [Layout] -> [[Layout]]
        tmp1 [] = []
        tmp1 xs = take 6 xs : tmp1 (drop 6 xs)

        tmp2 :: [[Layout]] -> [[Layout]]
        tmp2 = zipWith (:) $ map label ["1", "2", "3", "4", "5"]

        tmp3 = (map label ["", "月", "火", "水", "木", "金", "土"] :)

        lchoices = grid 5 5 $ (tmp3 . tmp2 . tmp1) $ map widget xs

createStaticTextPair :: Frame () -> Grade -> IO (StaticText (), StaticText ())
createStaticTextPair f g = (,) <$> (createStaticText . common) g <*> (createStaticText . special) g
    where
        createStaticText :: (Integral a, Show a) => a -> IO (StaticText ())
        createStaticText x = staticText f [text := show x]

type BeforeRecord = (StaticText (), StaticText ())
type AfterRecord = (StaticText (), StaticText ())
createRecordBoxed ::  BeforeRecord -> AfterRecord -> Layout
createRecordBoxed br ar = boxed "成績" $ column 10 createRow
    where
        createRow = zipWith tmp ["共通", "専門"] fstOrSnd
            where
                fstOrSnd = map (widget .) [fst, snd]
                tmp t f = row 5 [label t, f br, label " -> ", f ar]
