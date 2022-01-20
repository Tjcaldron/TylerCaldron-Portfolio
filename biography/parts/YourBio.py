class displayMenu:
    def displayOptions():
        print('Menu')
        print('   + - Add to the bio')
        print('   d - Display current bio')
        print('   e - Edit an item')
        print('   c - Check or remove item from list')
        print('   ? - Display this menu')
        print('   q - Quit pogram')



    
def displayBio(current_Bio):
    print('{:*<9}'.format('*'), "shopping list", '{:*>9}'.format('*'))
    for i in current_Bio:
        print('{:*<1}'.format('*'), '{}'.format(i), '{: >25}'.format('*'))
    print('{:*<33}'.format('*'))

   

