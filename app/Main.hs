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
    student        <- runRelation conn selectAllFromStudentWhereStudentNumber 12345
    lectures       <- mapM (runRelation conn selectAllFromLectureWherePeriod) period
    grades         <- runRelation conn selectAllFromGradeWhereStudentId $ S.studentId (head student)

    f              <- frame [text := "履修登録"]
    lectureChoices <- createLectureChoices f lectures
    button1        <- createRegistrationBotton f
    before@(beforeCommonValue, beforeSpecilizedValue) <- createStaticTextPair f $ head grades
    after@(afterCommonValue, afterSpecilizedValue)    <- createStaticTextPair f $ head grades

    Graphics.UI.WX.set f
        [ layout := column 10
            [ createSemesterInformation "秋"
            , createStudentInformation $ head student
            , createRegistrationBoxed (map fst lectureChoices) button1
            , createRecordBoxed before after
            ]
        ]

    choiceAddHanlders <- mapM newChoiceAddHandler lectureChoices
    buttonAddHandler  <- newButtonAddHandler button1


    let netWorkDescription :: MomentIO ()
        netWorkDescription = mdo

            -- Event
            (echoices :: [Event [Maybe Lecture]]) <- mapM fromAddHandler choiceAddHanlders
            (ebutton  :: Event ())                <- fromAddHandler buttonAddHandler

            -- Behavior
            (bcommon     :: Behavior String) <- fromPoll $ get beforeCommonValue text
            (bspecilized :: Behavior String) <- fromPoll $ get beforeSpecilizedValue text

            let echoices'  = foldr (\e acc -> unionWith (++) e acc) never echoices            :: Event [Maybe Lecture]
                echoices1  = fmap ((filter (\l -> L.field l == "共通")) . catMaybes) echoices' :: Event [Lecture]
                echoices2  = fmap ((filter (\l -> L.field l == "専門")) . catMaybes) echoices' :: Event [Lecture]
                echoices1' = fmap ((foldr (+) 0) . (map (fromIntegral . L.credit))) echoices1 :: Event Int
                echoices2' = fmap ((foldr (+) 0) . (map (fromIntegral . L.credit))) echoices2 :: Event Int

            (bchoices1 :: Behavior Int) <- stepper 0 echoices1'
            (bchoices2 :: Behavior Int) <- stepper 0 echoices2'

            let bcommonResult     = (+) <$> fmap read bcommon     <*> bchoices1
                bspecilizedResult = (+) <$> fmap read bspecilized <*> bchoices2

            bcommonValue <- valueBLater bcommonResult
            bspecilizedValue <- valueBLater bspecilizedResult

            liftIOLater $ set afterCommonValue [text := show bcommonValue]
            liftIOLater $ set afterSpecilizedValue [text := show bspecilizedValue]

            changeCommon      <- changes bcommonResult
            changeSpecialized <- changes bspecilizedResult

            reactimate' $ (fmap (\x -> set afterCommonValue [text := show x]))     <$> changeCommon
            reactimate' $ (fmap (\x -> set afterSpecilizedValue [text := show x])) <$> changeSpecialized

            -- Eventの生成
            -- echoices    <- mapM (`event0` select) choices
            -- ebutton     <- event0 button1 command

            -- Behaviorの生成
            -- bchoices    <- mapM (`behavior` selection) choices
            -- bcommon     <- behavior beforeCommonValue text
            -- bspecilized <- behavior beforeSpecilizedValue text
            --
            -- let
            --     -- ボタンイベントが発火した時、履修する講義をデータベースにInsertする。
            --     buttonEvent :: Event (IO ())
            --     buttonEvent = fmap (\_ -> action) ebutton
            --         where
            --             action :: IO ()
            --             action = undefined
            --
            --     -- 選択した講義を返す
            --     fetchLecture :: [Lecture] -> Int -> Maybe Lecture
            --     fetchLecture _ 0  = Nothing
            --     fetchLecture ls c = Just (ls !! (c - 1))
            --
            --     -- 選択した講義の単位数を返す
            --     fetchCredit :: String -> Maybe Lecture -> Int
            --     fetchCredit s = maybe 0 tmp
            --         where
            --             tmp :: Lecture -> Int
            --             tmp l = if field l == s then fromIntegral . credit $ l else 0
            --
            --     -- 分野を受け取り、現在の総単位数と履修する講義の総単位数との和を返す
            --     result :: String -> Behavior Int
            --     result z = result' behaviorCommonAndSpecilized
            --         where
            --             bchoices' = zipWith imposeChanges bchoices echoices
            --
            --             behaviorCommonAndSpecilized :: Behavior String
            --             behaviorCommonAndSpecilized = case z of
            --                 "共通" -> bcommon
            --                 "専門" -> bspecilized
            --                 z' -> error $ z' ++ " is undefined Field value."
            --
            --             result' :: Behavior String -> Behavior Int
            --             result' o = foldr (\a b -> (+) <$> a <*> b) (read <$> o) bs'
            --                 where
            --                     bs' = zipWith (\x y -> (fetchCredit z . fetchLecture x) <$> y) lectures bchoices'
            --
            --
            -- sink afterCommonValue [text :== show <$> result "共通"]
            -- sink afterSpecilizedValue [text :== show <$> result "専門"]


    network <- compile netWorkDescription
    actuate network
    return ()

    where
        xs = ["A"]
        ys = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        zs = ["1", "2", "3", "4", "5"]
        period :: [String]
        period = (\x y z -> x ++ y ++ z) <$> xs <*> ys <*> zs

newChoiceAddHandler :: LectureChoice -> IO (AddHandler [Maybe Lecture])
newChoiceAddHandler (c, ls) = do
    (choiceAddHandler, choiceRunHandler) <- newAddHandler
    set c [on select := choiceAction >>= choiceRunHandler]
    return choiceAddHandler
    where
        choiceAction :: IO ([Maybe Lecture])
        choiceAction = get c selection >>= return . (\x -> if x == 0 then [Nothing] else [Just (ls !! (x - 1))])

newButtonAddHandler :: Button () -> IO (AddHandler ())
newButtonAddHandler b = do
    (buttonAddHandler, buttonRunHandler) <- newAddHandler
    set b [on command := buttonAction >>= buttonRunHandler]
    return buttonAddHandler
    where
        buttonAction :: IO ()
        buttonAction = return ()
