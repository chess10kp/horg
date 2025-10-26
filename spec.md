# Org Mode Language Specification

This document outlines the targeted syntax elements for parsing Org mode markup into a JSON representation.

## Core Elements

### Headlines
- Syntax: One or more asterisks (*) followed by a space and the headline text.
- Levels: Determined by the number of asterisks (1-10 typically).
- Example: * Headline, ** Subheadline.

### Text and Formatting
- Plain text: Any text not matching other patterns.
- Bold: *text* (asterisks around text).
- Italic: /text/ (slashes around text).
- Underline: _text_ (underscores around text).
- Code: =text= (equals around text).
- Verbatim: ~text~ (tildes around text).
- Strike-through: +text+ (plus signs around text).

### Lists
- Unordered: Lines starting with - or + followed by space.
- Ordered: Lines starting with 1. or 1) followed by space.
- Nested lists: Indented sub-items.

### Links
- Syntax: [[link][description]] or [[link]].
- Internal links: [[#anchor]] or [[file:filename]].

### Code Blocks
- Syntax: #+BEGIN_SRC language ... #+END_SRC.
- Optional parameters: :exports, :results, etc.

### Keywords
- Lines starting with #+ followed by a keyword, e.g., #+TITLE:, #+AUTHOR:.

### Properties
- Syntax: :PROPERTIES: ... :END: with key-value pairs.

### Tables
- Syntax: | cell | cell | ... for rows.
- Alignment: |<l> or |<c> or |<r>.

### Comments
- Lines starting with # (single #, not #+).

## Parsing Goals
- Parse into an AST representing the document structure.
- Convert AST to JSON for output.
- Handle nested structures like sub-headlines and nested lists.
- Ignore or handle unknown elements gracefully.