
module Main
    ( main
    )where

import           Control.Applicative
import           Control.Monad
import           Database.Relations.Course  as C
import           Database.Relations.Grade   as G
import           Database.Relations.Lecture as L
import           Database.Relations.Student as S
import           Graphics.UI.WX             hiding (Event)
import           Library.AccessDatabase
import           Library.Interface
import           Reactive.Banana
import           Reactive.Banana.WX

-- | PostgreSQLサーバから学生, 講義の情報を取り出し、インターフェースを構築する。
main :: IO ()
main = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> start $ do
    student        <- runRelation conn selectAllFromStudentWhereStudentNumber 12345
    lectures       <- mapM (runRelation conn selectAllFromLectureWherePeriod) period
    grades <- runRelation conn selectAllFromGradeWhereStudentId $ S.studentId (head student)

    f              <- frame [text := "履修登録"]
    choices <- createLectureChoices f lectures
    before@(beforeCommonValue, beforeSpecilizedValue) <- createStaticTextPair f $ head grades
    after@(afterCommonValue, afterSpecilizedValue) <- createStaticTextPair f $ head grades

    Graphics.UI.WX.set f
        [ layout := column 10
            [ createSemesterInformation "秋"
            , createStudentInformation $ head student
            , createRegistrationBoxed choices
            , createRecordBoxed before after
            ]
        ]

    let netWorkDescription :: MomentIO ()
        netWorkDescription = do
            echoices <- mapM (`event0` select) choices
            bchoices <- mapM (`behavior` selection) choices
            bcommon <- behavior beforeCommonValue text
            bspecilized <- behavior beforeSpecilizedValue text

            let
                fetchLecture :: [Lecture] -> Int -> Maybe Lecture
                fetchLecture _ 0  = Nothing
                fetchLecture ls c = Just (ls !! (c - 1))

                fetchCredit :: String -> Maybe Lecture -> Int
                fetchCredit s = maybe 0 tmp
                    where
                        tmp :: Lecture -> Int
                        tmp l = if field l == s then fromIntegral . credit $ l else 0

                result :: String -> Behavior Int
                result z = result' behaviorCommonAndSpecilized
                    where
                        bchoices' = zipWith imposeChanges bchoices echoices

                        behaviorCommonAndSpecilized :: Behavior String
                        behaviorCommonAndSpecilized = case z of
                            "共通" -> bcommon
                            "専門" -> bspecilized

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
