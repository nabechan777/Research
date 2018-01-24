DROP TABLE IF EXISTS grade;
DROP TABLE IF EXISTS course;
DROP TABLE IF EXISTS student;
DROP TABLE IF EXISTS lecture;

CREATE TABLE student(
    student_id int PRIMARY KEY,
    student_number int NOT NULL,
    name varchar(20) NOT NULL,
    faculty varchar(20) NOT NULL,
    department varchar(20) NOT NULL,
    semester varchar(3) NOT NULL
);

-- 春=S, 秋=A
-- 月=Mon, 火=Tue, 水=Wed, 木=Thu, 金=Fri, 土=Sat, 日=Sun
-- 時限=1..5
CREATE TABLE lecture(
    lecture_id int PRIMARY KEY,
    name varchar(20) NOT NULL,
    period varchar(5) NOT NULL,
    field varchar(5) NOT NULL,
    credit int NOT NULL
);

CREATE TABLE course(
    course_id int PRIMARY KEY,
    student_id int NOT NULL,
    lecture_id int NOT NULL,
    score int,
    FOREIGN KEY (student_id) REFERENCES student(student_id),
    FOREIGN KEY (lecture_id) REFERENCES lecture(lecture_id)
);

CREATE TABLE grade (
    grade_id int PRIMARY KEY,
    student_id int NOT NULL,
    common int NOT NULL,
    special int NOT NULL,
    FOREIGN KEY (student_id) REFERENCES student(student_id)
);
