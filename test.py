

def makeTest(n):
    varss = ""
    suma = ""
    for i in range(n):
	varss += "\tvar a" + str(i) + ":= 1\n"
        suma += "a" + str(i) + " + " 
   
    varss += "\tvar a" + str(n) + ":= 1\n"  
    suma += "a" + str(n)
    res = "function suma () : int =\n let\n" + varss + "in\n" + suma + "\nend"
    return res


if __name__ == "__main__":
	print "let"
        print makeTest(100)
        print "in"
        print "\t suma()"
	print "end"
	
