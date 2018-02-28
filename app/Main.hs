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
import           Library.FRP
import           Reactive.Banana
import           Reactive.Banana.WX
import           Data.Maybe

-- | PostgreSQLサーバから学生, 講義の情報を取り出し、インターフェースを構築する。
main :: IO ()
main = handleSqlError' $ withConnectionIO' (connectPostgreSQL "dbname=research") $ \conn -> start $ do
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

    choiceAddHandler <- newLectureChoiceAddHandler lectureChoices
    buttonAddHandler <- newButtonAddHandler button1

    let sumOfLectureCredit :: String -> [Lecture] -> Int
        sumOfLectureCredit x = (foldr (+) 0) . (map (fromIntegral . L.credit)) . (filter (\l -> L.field l == x))

        sumOfCommonLectureCredit :: [Lecture] -> Int
        sumOfCommonLectureCredit = sumOfLectureCredit "共通"

        sumOfSpecilizedLectureCredit :: [Lecture] -> Int
        sumOfSpecilizedLectureCredit = sumOfLectureCredit "専門"

        -- netWorkDescription :: MomentIO ()
        -- netWorkDescription = mdo
        --
        --     -- Eventの生成
        --     ebutton     <- event0 button1 command
        --
        --     -- Behaviorの生成
        --     bchoice <- fromChanges [] choiceAddHandler
        --     bcommon     <- behavior beforeCommonValue text
        --     bspecilized <- behavior beforeSpecilizedValue text
        --
        --     let bchoice' = fmap catMaybes bchoice
        --         bres1 = (+) <$> fmap read bcommon <*> fmap sumOfCommonLectureCredit bchoice'
        --         bres2 = (+) <$> fmap read bspecilized <*> fmap sumOfSpecilizedLectureCredit bchoice'
        --
        --     -- 出力
        --     reactimate' =<< buttonAction student bchoice' ebutton
        --     sink afterCommonValue [text :== show <$> bres1]
        --     sink afterSpecilizedValue [text :== show <$> bres2]

        netWorkDescription :: MomentIO ()
        netWorkDescription = mdo
            -- Event
            echoice <- fromAddHandler choiceAddHandler
            ebutton <- fromAddHandler buttonAddHandler

            -- Behavior
            bchoice <- stepper [] echoice'
            bcommon <- fromPoll $ get beforeCommonValue text
            bspecilized <- fromPoll $ get beforeSpecilizedValue text

            let echoice' = fmap catMaybes echoice
                eres1 = (+) <$> fmap read bcommon <@> fmap sumOfCommonLectureCredit echoice'
                eres2 = (+) <$> fmap read bspecilized <@> fmap sumOfSpecilizedLectureCredit echoice'

            reactimate $ (\x -> set afterCommonValue [text := show x]) <$> eres1
            reactimate $ (\x -> set afterSpecilizedValue [text := show x]) <$> eres2
            reactimate $ (\x -> registrationAction student x) <$> (bchoice <@ ebutton)

    network <- compile netWorkDescription
    actuate network
    return ()

    where
        xs = ["A"]
        ys = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        zs = ["1", "2", "3", "4", "5"]
        period :: [String]
        period = (\x y z -> x ++ y ++ z) <$> xs <*> ys <*> zs
