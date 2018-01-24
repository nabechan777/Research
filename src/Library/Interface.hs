module Library.Interface
    ( createSemesterInformation
    , createStudentInformation
    , createChoices
    , createRegistrationBoxed
    )where

import           Control.Applicative
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
createChoices :: Frame () -> OneWeekLecture -> IO [Choice ()]
createChoices f xs = mapM (\x -> choice f [items := "--" : map L.name x]) xs

createRegistrationBoxed :: [Choice ()] -> Layout
createRegistrationBoxed xs = boxed "履修" $ grid 5 5 $ (tmp3 . tmp2 . tmp1) $ map widget xs
    where
        tmp1 :: [Layout] -> [[Layout]]
        tmp1 [] = []
        tmp1 xs = take 6 xs : tmp1 (drop 6 xs)

        tmp2 :: [[Layout]] -> [[Layout]]
        tmp2 = zipWith (:) $ map label ["1", "2", "3", "4", "5"]

        tmp3 = (map label ["", "月", "火", "水", "木", "金", "土"] :)

type Specilized = Int
type Common = Int
createStaticTextPair :: Frame () -> (Common, Specilized) -> IO (StaticText (), StaticText ())
createStaticTextPair f (c, s) = undefined

type BeforeRecord = (StaticText (), StaticText ())
type AfterRecord = (StaticText (), StaticText ())
createRecordBoxed ::  BeforeRecord -> AfterRecord -> IO Layout
createRecordBoxed = undefined
