
module Main
    ( main
    )where

import Control.Monad
import Control.Applicative
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Library.AccessDatabase
import Library.Interface
import Database.Relations.Student as Student
import Database.Relations.Lecture as Lecture
import Database.Relations.Course as Course
import Database.Relations.Grade as Grade

-- | PostgreSQLサーバから学生, 講義の情報を取り出し、インターフェースを構築する。
main :: IO ()
main = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> start $ do
    student        <- runRelation conn selectAllFromStudentWhereStudentNumber 12345
    lectures       <- sequence $ map (\x -> runRelation conn selectAllFromLectureWherePeriod x) period
    grades <- runRelation conn selectAllFromGradeWhereStudentId $ Student.studentId (head student)

    f              <- frame [text := "履修登録"]
    choices        <- sequence $ map (\l -> choice f [items := ("--" : map Lecture.name l)]) lectures
    beforeCommonValue <- staticText f [text := (show . common . head) $ grades]
    beforeSpecilizedValue <- staticText f [text := (show . special . head) $ grades]
    afterCommonValue    <- staticText f [text := (show . common . head) $ grades]
    afterSpecilizedValue <- staticText f [text := (show . special . head) $ grades]

    Graphics.UI.WX.set f [layout := column 10 [
                             createSemesterInformation "秋"
                            ,createStudentInformation $ head student
                            ,boxed "履修" $ grid 5 5 [
                                 map label ["", "月", "火", "水", "木", "金", "土"]
                                ,label "1" : take 6 (map widget choices)
                                ,label "2" : take 6 (drop 6  (map widget choices))
                                ,label "3" : take 6 (drop 12 (map widget choices))
                                ,label "4" : take 6 (drop 18 (map widget choices))
                                ,label "5" : take 6 (drop 24 (map widget choices))
                            ]
                            ,boxed "成績" $ column 10 [
                                row 5 [label "共通: ", widget beforeCommonValue, label " -> ", widget afterCommonValue],
                                row 5 [label "専門: ", widget beforeSpecilizedValue, label " -> ", widget afterSpecilizedValue]
                            ]]]

    let netWorkDescription :: MomentIO ()
        netWorkDescription = do
            echoices <- mapM (\c -> event0 c select) choices :: MomentIO [Event ()]
            bchoices <- mapM (\c -> behavior c selection) choices :: MomentIO [Behavior Int]
            bcommon <- behavior beforeCommonValue text
            bspecilized <- behavior beforeSpecilizedValue text

            let
                fetchLecture :: [Lecture] -> Int -> Maybe Lecture
                fetchLecture _ 0  = Nothing
                fetchLecture ls c = Just (ls !! (c - 1))

                fetchCredit :: String -> Maybe Lecture -> Int
                fetchCredit s ml = maybe 0 tmp ml
                    where
                        tmp :: Lecture -> Int
                        tmp l = if field l == s then fromIntegral . credit $ l else 0

                result :: String -> Behavior Int
                result z = result' behaviorCommonAndSpecilized
                    where
                        bchoices' = zipWith (\b e -> imposeChanges b e) bchoices echoices

                        behaviorCommonAndSpecilized :: Behavior String
                        behaviorCommonAndSpecilized = case z of
                            "共通" -> bcommon
                            "専門" -> bspecilized
                            otherwise -> undefined

                        result' :: Behavior String -> Behavior Int
                        result' o = foldr (\a b -> (+) <$> a <*> b) (read <$> o) bs'
                            where
                                bs' = zipWith (\x y -> (fetchCredit z . fetchLecture x) <$> y) lectures bchoices'


            sink afterCommonValue [text :== show <$> result "共通"]
            sink afterSpecilizedValue [text :== show <$> result "専門"]


    network <- compile netWorkDescription
    actuate network
    return ()

    where
        xs = ["A"]
        ys = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        zs = ["1", "2", "3", "4", "5"]
        period :: [String]
        period = (\x y z -> x ++ y ++ z) <$> xs <*> ys <*> zs
