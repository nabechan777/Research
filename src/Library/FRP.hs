module Library.FRP
    ( buttonAction
    , newLectureChoiceAddHandler
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.UI.WX hiding (Event)
import Database.Relations.Lecture as L
import Database.Relations.Student as S
import Database.Relations.Course as C
import Library.Interface
    ( LectureChoice
    )
import Library.AccessDatabase
import Database.HDBC.Record
    ( mapInsert
    )
import Database.HDBC
    ( commit
    )

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

newLectureChoiceAddHandler :: [LectureChoice] -> IO (AddHandler [Maybe Lecture])
newLectureChoiceAddHandler lcs = do
    (addHandler, runHandler) <- newAddHandler
    let choices = map fst lcs
    mapM_ (\c -> set c [on select := mapM selectedLecture lcs >>= runHandler]) choices
    return addHandler
    where
        selectedLecture :: LectureChoice -> IO (Maybe Lecture)
        selectedLecture (c, ls) = get c selection >>= (\n -> return $ if n == 0 then Nothing else Just (ls !! (n - 1)))

newLectureChoiceBehavior :: [LectureChoice] -> MomentIO (Behavior [Maybe Lecture])
newLectureChoiceBehavior lcs = fromPoll $ mapM action lcs
    where
        action :: LectureChoice -> IO (Maybe Lecture)
        action (c, ls) = get c selection >>= (\n -> return $ if n == 0 then Nothing else Just (ls !! (n - 1)))
        -- tmp :: [LectureChoice] -> IO [Maybe Lecture]
        -- tmp lcs = mapM (\(c,ls) -> get c selection >>= (\n -> return $ if n == 0 then Nothing else Just (ls !! (n - 1)))) lcs
