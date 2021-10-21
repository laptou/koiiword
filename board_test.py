
def add_word(letters, word, direction, start):
  row, col = start
  for letter in word:
    position = (row, col)
    if position not in letters:
      letters[position] = letter
    elif letters[position] != letter:
      raise IndexError

    if direction == 'h':
      col += 1
    elif direction == 'v':
      row += 1
    else:
      raise ValueError

def print_board(letters):
  bounds = (
    (
      min(row for row, _ in letters.keys()),
      min(col for _, col in letters.keys())
    ),
    (
      max(row for row, _ in letters.keys()),
      max(col for _, col in letters.keys())
    )
  )

  for row in range(bounds[0][0], bounds[1][0] + 1):
    for col in range(bounds[0][1], bounds[1][1] + 1):
      position = (row, col)
      if position in letters:
        print(letters[position], end = ' ')
      else:
        print(' ', end = ' ')
    print()
      
def get_words(letters):
  letters = dict(letters)
  explored = set()

  def get_attached_words(start, position, direction, search):
    row, col = position
      
    explored.add(position)

    current_letter = letters[position]
    current_word = '' if search else current_letter
    branch_words = []

    above = (row - 1, col)
    below = (row + 1, col)
    left =  (row, col - 1)
    right = (row, col + 1)

    if direction == 'v':
      if search and above in letters:
          return get_attached_words(start, above, 'v', True)
      else:
        if below in letters:
          partial_word, others = get_attached_words(start, below, 'v', False)
          current_word = current_letter + partial_word
          branch_words.extend(others)

        if right not in explored and right in letters:
          word, others = get_attached_words(position, right, 'h', True)
          branch_words.append(word)
          branch_words.extend(others)
        elif left not in explored and left in letters:
          word, others = get_attached_words(position, left, 'h', True)
          branch_words.append(word)
          branch_words.extend(others)
    else:
      if search and left in letters:
          return get_attached_words(start, left, 'h', True)
      else:
        if right in letters:
          partial_word, others = get_attached_words(start, right, 'h', False)
          current_word = current_letter + partial_word
          branch_words.extend(others)

        if below not in explored and below in letters:
          word, others = get_attached_words(position, below, 'v', True)
          branch_words.append(word)
          branch_words.extend(others)
        elif above not in explored and above in letters:
          word, others = get_attached_words(position, above, 'v', True)
          branch_words.append(word)
          branch_words.extend(others)

    return current_word, branch_words
    
  word, branches = get_attached_words(None, (0, 0), 'v', True)

  if word != '':
    return [word] + branches
  else:
    return branches
    

letters = dict()
add_word(letters, 'ablate', 'h', (0, 0))
add_word(letters, 'abrasive', 'v', (0, 0))
add_word(letters, 'hat', 'v', (-2, 4))
add_word(letters, 'inflate', 'v', (-3, 2))
add_word(letters, 'though', 'h', (2, 2))
add_word(letters, 'incinerate', 'h', (3, -7))
add_word(letters, 'arab', 'h', (3, -7))

print_board(letters)
print(get_words(letters))
print()

letters = dict()
add_word(letters, 'abrasive', 'v', (-2, 0))
add_word(letters, 'bootlicker', 'h', (-1, 0))

print_board(letters)
print(get_words(letters))
print()

letters = dict()
add_word(letters, 'abrasive', 'h', (0, 0))

print_board(letters)
print(get_words(letters))
print()

letters = dict()
add_word(letters, 'eat', 'v', (-2, 0))

print_board(letters)
print(get_words(letters))
print()

letters = dict()
add_word(letters, 'eat', 'h', (0, -2))

print_board(letters)
print(get_words(letters))
print()
