
from .bidi_brackets_parser import parse_bidi_brackets
from .bidi_mirroring_parser import parse_bidi_mirroring
from .composition_exclusions_parser import parse_composition_exclusions
from .east_asian_width_parser import parse_east_asian_width
from .emoji_data_parser import parse_emoji_data
from .grapheme_break_property_parser import parse_grapheme_break_property
from .line_break_parser import parse_line_break
from .scripts_parser import parse_scripts
from .sentence_break_property_parser import parse_sentence_break_property
from .unicode_data_parser import parse_unicode_data
from .word_break_property_parser import parse_word_break_property

from .hangul_decompositions import add_hangul_decompositions

from .description import initialize_descriptions
from .chunk import deduplicate_chunks
from .bits_as_bytes import bits_as_bytes, max_num_bits

from .composition_table import make_composition_table

from .output import generate_output
