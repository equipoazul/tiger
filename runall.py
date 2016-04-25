import os

if __name__ == '__main__':
    direc = os.getcwd() + '/tests/good/'
    files = os.listdir(direc)
    files.sort()
    import pdb; pdb.set_trace()
    for fil in files:
        print "=============================================================="
        print "                    EJECUTANDO %s                             " % fil
        print "=============================================================="
        print os.getcwd() + "/tiger  " + direc + fil
        os.system(os.getcwd() + "tiger  " + direc + fil)

