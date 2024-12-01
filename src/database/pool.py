import logging
from typing import Any, List, Tuple

import psycopg2
from psycopg2.pool import SimpleConnectionPool
from psycopg2.extras import DictCursor, DictRow

LOGGER = logging.getLogger("db.pool")


class ConnectionPool:
    def __init__(self, dbname: str, user: str, password: str, host: str = 'localhost', port: int = 5432):
        """Initialize database connection."""
        try:
            LOGGER.info(f"Connecting to database {dbname}@{host}:{port}")
            self.pool = SimpleConnectionPool(
                dbname=dbname, user=user, password=password, host=host, port=port,
                minconn=1, maxconn=16
            )
        except psycopg2.Error as e:
            raise Exception(f"Unable to connect to database: {e}")

    def close(self):
        self.pool.closeall()

    def execute(self, query: str, args: List[str] = ()) -> List[dict[Any, ...]] | None:
        """
        Execute an SQL query and fetch the results.

        This method retrieves a connection from the connection pool, executes the provided SQL
        query with given arguments, commits the transaction and returns the results.
        If no results are returned after execution, it returns None.
        Any other SQL error raises an exception and performs a rollback.

        :param query: The SQL query to be executed
        :type query: str

        :param args: The arguments to be passed to the SQL query
        :type args: List[str]

        :return: The results of the query as a list of dictionary rows, or None if there is no result
        :rtype: List[dict[Any, ...]] | None

        :raises Exception: If unable to execute the query due to an error
        """
        conn = self.pool.getconn()
        try:
            with conn.cursor(cursor_factory=DictCursor) as cur:
                cur.execute(query, args)
                conn.commit()

                result = cur.fetchall()
                return [dict(item) for item in result]

        except psycopg2.ProgrammingError:
            return None

        except psycopg2.Error as e:
            conn.rollback()
            raise Exception(f"Unable to execute query: {e}")
        finally:
            self.pool.putconn(conn)
