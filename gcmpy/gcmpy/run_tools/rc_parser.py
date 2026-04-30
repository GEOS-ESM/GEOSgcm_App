from pathlib import Path
import re

def update_end_date(file_path: Path, date: str, time: str) -> None:
    new_line = f"END_DATE: {date} {time}"

    text = file_path.read_text()

    updated_text = re.sub(
        r"^\s*END_DATE:.*$",
        new_line,
        text,
        flags=re.MULTILINE
    )

    file_path.write_text(updated_text)

def update_num_readers(file_path: Path, value: int) -> None:
    text = file_path.read_text()

    updated_text = re.sub(
        r"^(NUM_READERS.*?)(\d+)",
        rf"\g<1>{value}",
        text,
        flags=re.MULTILINE
    )

    file_path.write_text(updated_text)

def update_domain_start(file_path: Path, date: str, time: str) -> None:
    new_line = f"DOMAIN%STOP = '{date} {time}'"

    text = file_path.read_text()

    updated_text = re.sub(
        r"^\s*DOMAIN%START.*$",
        new_line,
        text,
        flags=re.MULTILINE
    )

    file_path.write_text(updated_text)


def update_domain_stop(file_path: Path, date: str, time: str) -> None:
    new_line = f"DOMAIN%STOP = '{date} {time}'"

    text = file_path.read_text()

    updated_text = re.sub(
        r"^\s*DOMAIN%STOP.*$",
        new_line,
        text,
        flags=re.MULTILINE
    )

    file_path.write_text(updated_text)

