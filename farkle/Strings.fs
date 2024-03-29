namespace farkle

module Strings =
    let titleString = "Welcome to Farkle"
    let errorAmountString = "\nEnter an amount greater than or equal to 5"
    let errorPlayerAmountString = "\nThe number of players must be greater than or equal to 2"

    let errorInvalidInputString = "\nThat was not a valid choice"

    let menuString =
        "\nWhat would you like to do?\n"
        + "[1] Play game (with default settings)\n"
        + "[2] Change settings\n"
        + "[3] Quit\n"

    let settingsMenuString =
        "\n[1] Change dice\n"
        + "[2] Change amount to roll\n"
        + "[3] Back to menu\n"

    let chooseDiceString = "\nChoose dice?"

    let amountToRollString = "\nEnter amount of dice to roll (5+)"

    let amountOfPlayersString = "\nEnter the amount of players (2+)"
    
    let enterPlayerNameString = "\nEnter player name"
    
    let pressEnterString = "Press enter to continue"