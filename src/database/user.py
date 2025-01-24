from typing import Dict, List, Optional

from src.const import DATABASE


def init_db():
    DATABASE.execute("""
                CREATE TABLE IF NOT EXISTS Users
                (
                    id      text NOT NULL PRIMARY KEY,
                    enabled boolean NOT NULL DEFAULT FALSE,
                    lang  text NOT NULL DEFAULT 'zh-tw'
                );
            """)


def create(user_id: str):
    DATABASE.execute(
        "INSERT INTO Users (id, enabled) VALUES (%s, False) ON CONFLICT (id) DO NOTHING",
        (user_id,)
    )


def remove(user_id: str):
    DATABASE.execute("DELETE FROM Users WHERE id = %s", (user_id,))


def toggle_enabled(user_id: str):
    result = DATABASE.execute(
        "UPDATE Users SET enabled = NOT enabled WHERE id = %s RETURNING enabled",
        (user_id,),
    )

    return result[0]["enabled"]


def get_enabled() -> Dict[str, List[str]]:
    result = DATABASE.execute("SELECT id, lang FROM Users WHERE enabled = TRUE")
    users = {}
    for d in result:
        users.get(d["lang"], []).append(d["id"])

    return users


def get_all() -> Dict[str, List[str]]:
    result = DATABASE.execute("SELECT id, lang FROM Users")
    users = {}
    for d in result:
        users.get(d["lang"], []).append(d["id"])

    return users


def get_lang(user_id: str) -> Optional[str]:
    result = DATABASE.execute(
        "SELECT lang FROM Users WHERE id = %s",
        (user_id,)
    )
    return result[0]["lang"] if result else None


def set_lang(user_id: str, lang: str):
    DATABASE.execute(
        "UPDATE Users SET lang = %s WHERE id = %s",
        (lang, user_id)
    )
