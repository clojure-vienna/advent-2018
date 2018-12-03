# How to participate

1. Get your test inputs at the [advent of code website](https://adventofcode.com/2018/)
2. For each day n, create your solutions at `src/clojure_vienna/advent_2018/{{your-username}}/day_{{n}}.clj`
3. Send a pull request :-)

# How to use the custom runner

1. run `lein repl` in the project directory. this will start a file watcher for your daily challenges
2. put your daily input at `resources/clojure-vienna/advent-2018/{{your-username}}/day-{{n}}`
3. in each solution, register a function, taking a list of lines and returning a vector of [result-1 result-2]
    ```
    (main/register-solution!
     "{{your-username}}" {{n}}
     (fn [lines]
       [(calc-1 lines)
        (calc-2 lines)]))

    ```
4. after finishing a solution, register your results. this will ensure, that your solution keeps intact, even if you
   choose to refactor or beautify your code
    ```
    (main/register-solution!
      "{{your-username}}" {{n}} (juxt calc-1 calc-2)
      :result-1 470
      :result-2 790)

    ```
5. you can also set a `:line-parser`, to be mapped over your input lines or even an `:input-reader`, if you don't want line-based input

6. occasionally run the whole suite
   ```
   $ export LEIN_FAST_TRAMPOLINE=1
   $ lein trampoline run
   ```
