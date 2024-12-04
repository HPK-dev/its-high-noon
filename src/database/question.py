import dataclasses
import logging
from typing import Optional, Dict, Any, List

from src.const import DATABASE

LOGGER = logging.getLogger("db.question")

@dataclasses.dataclass
class Question:
    id: str
    subject: str
    description: str
    opts: str
    ans: str
    explanation: Optional[str]
    details: Optional[str]

    @classmethod
    def from_dict(cls, question_dict: Dict[str, Any]) -> "Question":
        return cls(**question_dict)

    def make_question(self):
        return f"{self.description}\n\n{self.opts}"

    def make_answer(self):
        return f"Ans:{self.ans}\n\n{self.explanation}"

    def make_full(self):
        return f"{self.make_question()}\n\n{self.make_answer()}\n\n{self.details}"

    def verify_answer(self, answer: str) -> bool:
        """Checks if the answer is correct."""
        answer = answer.strip()
        return answer == self.ans.lower()


def init_db():
    DATABASE.execute("""
                CREATE TABLE IF NOT EXISTS Questions
                (
                    id          INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
                    subject     text NOT NULL,
                    description text NOT NULL,
                    opts        text NOT NULL,
                    ans         text NOT NULL,
                    explanation text,
                    details     text
                );
                """)


def create(subject: str, description: str, opts: str,
           ans: str, explanation: Optional[str] = None,
           details: Optional[str] = None):
    """
    Create a new question in the database.
    """

    LOGGER.debug(f"Creating question")

    DATABASE.execute(
        """
            INSERT INTO questions (subject, description, opts, ans, explanation, details)
            VALUES (%s, %s, %s, %s, %s, %s)
        """,
        (
            subject,
            description,
            opts,
            ans,
            explanation,
            details
        ))


def get(question_id: int) -> Optional[Question]:
    """Retrieve a question by its ID."""

    result = DATABASE.execute("SELECT * FROM questions WHERE id = %s", (question_id,))
    question = result[0]

    if question:
        return Question.from_dict(question)
    return None


def update(question_id: int, **kwargs):
    """
    Update a question's fields.
    Pass any field you want to update as a keyword argument.
    """
    valid_fields = {'subject', 'description', 'opts', 'ans', 'explanation', 'details'}
    update_fields = {k: v for k, v in kwargs.items() if k in valid_fields}

    if not update_fields:
        return False

    set_clause = ', '.join([
        f"{field} = %s" for field in update_fields.keys()
    ])
    values = list(update_fields.values())

    query = f"""
            UPDATE questions 
            SET {set_clause}
            WHERE id = %s
        """

    DATABASE.execute(query, values + [question_id])


def delete(question_id: int):
    """Delete a question by its ID."""

    query = "DELETE FROM questions WHERE id = %s"
    DATABASE.execute(query, (question_id,))


def get_with_subject(subject: str) -> list[Question]:
    """Retrieve all questions for a given subject."""

    query = "SELECT * FROM questions WHERE subject = %s ORDER BY id"
    result = DATABASE.execute(query, (subject,))

    return [Question.from_dict(dict(q)) for q in result]


def search(search_term: str) -> list[Question]:
    """Search questions by subject or description."""

    query = """
            SELECT * FROM questions 
            WHERE subject ILIKE %s OR description ILIKE %s
            ORDER BY id
            """

    search_pattern = f'%{search_term}%'
    result = DATABASE.execute(query, (search_pattern, search_pattern))

    return [Question.from_dict(q) for q in result]


RETURNED_QUESTIONS = set()


def random_one(reset_if_exhausted: bool = True) -> Optional[Question]:
    """
    Retrieve a random question that hasn't been returned before.

    Args:
        reset_if_exhausted (bool): If True, reset the tracking of returned questions
            when all questions have been returned.
            If False, return None when all questions have been returned.

    Returns:
        Optional[Dict[str, Any]]: A randomly selected question as a dictionary that
        hasn't been returned before, or None if all questions have been returned
        and reset_if_exhausted is False.

    Raises:
        Exception: If there's an error executing the database query.
    """

    # First, check the total number of questions
    result = DATABASE.execute("SELECT COUNT(*) FROM questions")
    total_questions = result[0]['count']

    # If all questions have been returned
    if len(RETURNED_QUESTIONS) >= total_questions:
        if reset_if_exhausted:
            RETURNED_QUESTIONS.clear()
        else:
            return None

    # If no questions have been returned yet, don't include WHERE clause
    if not RETURNED_QUESTIONS:
        result = DATABASE.execute("SELECT * FROM questions ORDER BY RANDOM() LIMIT 1")
    else:
        query = """
                    SELECT * FROM questions 
                    WHERE id NOT IN %(returned_ids)s
                    ORDER BY RANDOM() 
                    LIMIT 1
                """
        result = DATABASE.execute(query, {'returned_ids': tuple(RETURNED_QUESTIONS)})


    if result:
        question = result[0]
        RETURNED_QUESTIONS.add(question['id'])
        return Question.from_dict(question)
    return None


def reset_returned_questions() -> None:
    """
    Reset the tracking of returned questions, allowing all questions to be returned again.
    """
    RETURNED_QUESTIONS.clear()


def get_all_subject() -> List[str]:
    """Retrieve all subjects."""

    result = DATABASE.execute("SELECT DISTINCT subject FROM questions")

    return [x for x, in result]
