# Codewords

## About the project
This is a project I built around a few years ago using Haskell, Reflex/Reflex-DOM, Obelisk and stylized with Tailwind-CSS. It is currently live at [codewordshaskell.com](https://codewordshaskell.com/). It is a real-time multiplayer game through the use of concurrency and network websockets (similar to the board game Codenames).

## How to Test/Play around
Since this is a multiplayer game you need at least 4 players to play the game. You can test this by yourself by opening up the link above in four different tabs.

### Game Rules
There are two teams, Red and Blue. Each team has a Speaker, and the rest of the team are guessers (or listeners, if it's not your turn to guess yet). The Speaker cannot talk to their team. The speakers can see the whole board and see which cards belong to their team and the opponent's team. Red/Blue cards belonging to their respective teams, Green belonging to spectators and the Black belonging to the assassin.

The speaker's job is to give their team a one word hint and the number of words that hint connects to. The guesser's game board starts out as all gray cards. One player from each team will be selected as the current guesser. They are allowed to guess up to the number of words the speaker has given plus 1 additional one (usually you would use this guess to try another card from a previous round that you might've failed to guess). You make a guess by selecting the word when prompted.

If the guesser guesses the opponent's card, their turn ends and the opponent gets the point. If the guesser guesses a green card (the Specator), their turn is ends and no one gets any points. Lastly, if the guesser guesses the black card (the Assassin), the game is over and the other team wins, so it is crucial for the speaker to make sure that this word does not get chosen. The goal of the game is to get all of your cards selected, one game consists of multiple rounds until either your team or the other team wins.
