class user_inputs:

    def addToBio(inputBio):
        inputBio.append(input('what would you like to add?: '))
        current_Bio = list(inputBio)

    def removeItem(current_Bio):
        checkItem = input("Which item do you want to check: ")
        if checkItem in current_Bio:
            current_Bio.remove(checkItem)
            print('Item was checked and removed')
        elif checkItem != current_Bio:
            print("Error: There is no item with that name")

    def editItem(itemList):
        replacement = input("Which item do you want to replace: ")
        if replacement in itemList:
            itemList.remove(replacement)
        elif replacement != itemList:
            print("Error: There is no item with that name")     