defmodule Main do
  def remove_vowels(vowels, word, n, new_word) do
    if n >= String.length(word) - 1 do
      new_word
    else
      current = String.at(word, n) |> String.downcase()

      if String.capitalize(current) in vowels do
        remove_vowels(vowels, word, n + 1, new_word)
      else
        remove_vowels(vowels, word, n + 1, new_word <> "." <> current)
      end
    end
  end

  def main do
    word = IO.gets("")
    vowels = ["A", "O", "Y", "E", "U", "I"]
    new_word = remove_vowels(vowels, word, 0, "")
    IO.puts(new_word)
  end
end

Main.main()
