import os
from typing import Optional

class Direction:
    FORWARD: "Direction"
    BACKWARD: "Direction"
    def __int__(self) -> int: ...

class TranslationResult:
    output: str
    emphasis: Optional[list[tuple[str, int, int]]]
    output_positions: Optional[list[int]]
    input_positions: Optional[list[int]]
    cursor_pos: Optional[int]

class Translator:
    def __init__(
        self,
        tables: list[str | os.PathLike],
        direction: Direction = ...,
    ) -> None: ...
    def translate(self, text: str) -> str: ...
    def translate_with_options(
        self,
        text: str,
        *,
        mode: int = ...,
        emphasis: Optional[list[tuple[str, int, int]]] = ...,
        cursor_pos: Optional[int] = ...,
    ) -> TranslationResult: ...

class LouisError(Exception): ...

class TableParseError(LouisError):
    errors: list[str]

class TranslationError(LouisError): ...

__version__: str
