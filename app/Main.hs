{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

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
import           Library.AccessDatabase     hiding (on, set)
import           Library.Interface
import           Reactive.Banana
import           Reactive.Banana.WX
import           Data.Maybe

-- | PostgreSQLサーバから学生, 講義の情報を取り出し、インターフェースを構築する。
main :: IO ()
main = handleSqlError' $ withConnectionIO (connectPostgreSQL "dbname=research") $ \conn -> start $ do
    (student:_)    <- runRelation conn selectAllFromStudentWhereStudentNumber 12345
    lectures       <- mapM (runRelation conn selectAllFromLectureWherePeriod) period
    (grades:_)     <- runRelation conn selectAllFromGradeWhereStudentId $ S.studentId student

    f              <- frame [text := "履修登録"]
    lectureChoices <- createLectureChoices f lectures
    button1        <- createRegistrationBotton f
    before@(beforeCommonValue, beforeSpecilizedValue) <- createStaticTextPair f grades
    after@(afterCommonValue, afterSpecilizedValue)    <- createStaticTextPair f grades

    Graphics.UI.WX.set f
        [ layout := column 10
            [ createSemesterInformation "秋"
            , createStudentInformation student
            , createRegistrationBoxed (map fst lectureChoices) button1
            , createRecordBoxed before after
            ]
        ]

    let netWorkDescription :: MomentIO ()
        netWorkDescription = mdo

            -- Eventの生成
            echoices    <- mapM (`event0` select) $ map fst lectureChoices
            ebutton     <- event0 button1 command

            let echoice = foldr (unionWith (\_ _ -> ())) never echoices

            -- Behaviorの生成
            bchoice <- newLectureChoiceBehavior lectureChoices
            bcommon     <- behavior beforeCommonValue text
            bspecilized <- behavior beforeSpecilizedValue text

            let bchoice' = fmap catMaybes $ imposeChanges bchoice echoice
                bchoice1 = fmap (filter (\l -> L.field l == "共通")) bchoice'
                bchoice2 = fmap (filter (\l -> L.field l == "専門")) bchoice'
                bchoice1' = fmap (map (fromIntegral . L.credit)) bchoice1
                bchoice2' = fmap (map (fromIntegral . L.credit)) bchoice2
                bsum1 = fmap (foldr (+) 0) bchoice1'
                bsum2 = fmap (foldr (+) 0) bchoice2'
                bres1 = (+) <$> fmap read bcommon <*> bsum1
                bres2 = (+) <$> fmap read bspecilized <*> bsum2

            -- 出力
            reactimate' =<< buttonAction student bchoice' ebutton
            sink afterCommonValue [text :== show <$> bres1]
            sink afterSpecilizedValue [text :== show <$> bres2]

    network <- compile netWorkDescription
    actuate network
    return ()

    where
        xs = ["A"]
        ys = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        zs = ["1", "2", "3", "4", "5"]
        period :: [String]
        period = (\x y z -> x ++ y ++ z) <$> xs <*> ys <*> zs


buttonAction :: Student -> Behavior [Lecture] -> Event () -> MomentIO (Event (Future (IO ())))
buttonAction s bchoice ebutton = do
    bchoice' <- stepper [] (bchoice <@ ebutton)
    changeChoice <- changes bchoice'
    return $ (fmap (\ls -> action s ls)) <$> changeChoice
    where
        -- 履修登録をする講義をデータベースに登録するアクション。リファクタリングの余地あり
        action :: Student -> [Lecture] -> IO ()
        action s ls = handleSqlError' $ withConnectionIO' (connectPostgreSQL "dbname=research") $ \conn -> do
            courses <- runRelation conn selectCourseIdFromCourse ()
            let courseIds = map C.courseId courses
                maxCourseId = (fromIntegral . maximum) $ courseIds
                nextCourseId = maxCourseId + 1

            let studentId = cycle [S.studentId s]
                lectureIds = map L.lectureId ls
                scores = cycle [Nothing]
                course = getZipList $ Course <$> ZipList [nextCourseId..]
                                             <*> ZipList studentId
                                             <*> ZipList lectureIds
                                             <*> ZipList scores
            mapInsert conn insertCourse course
            commit conn

newLectureChoiceBehavior :: [LectureChoice] -> MomentIO (Behavior [Maybe Lecture])
newLectureChoiceBehavior lcs = fromPoll $ tmp lcs
    where
        tmp :: [LectureChoice] -> IO [Maybe Lecture]
        tmp lcs = mapM (\(c,ls) -> get c selection >>= (\n -> return $ if n == 0 then Nothing else Just (ls !! (n - 1)))) lcs
