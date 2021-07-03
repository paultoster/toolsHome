import string
#
# Funktionen
#
# get_index_quot(text,quot0,quot1)
# get_index_no_quot(text,quot0,quot1)
# get_string_quoted(text,quot0,quot1)
# elim_comment_not_quoted(text,comment_list,quot0,quot1)    
# such(text,muster,regel)
# elim_a(text,muster) 
# elim_e(text,muster) 
# elim_ae(text,muster) 
# slice(text,length)
# split_not_quoted(text,split,quot0,quot1,elim_leer):
# split_with_quot(text,quot0,quot1):
#
QUOT    = 1
NO_QUOT = 0
def get_index_quot(text,quot0,quot1):
    """ Gibt Indexpaare (Tuples) in dem der Text gequotet ist z.B.
    
        text  = "abc {nest} efg  {plab}"
        #        0123456789012345678901
        quot0 = "{"
        quot1 = "}"
        
        a = get_index_quot(text,quot0,quot1)
        
        a ergibt [(5,9),(17,21)]
    """
    
    liste = []
    
    i0  = 0
    i1  = len(text)
#    print("i1 = %i" % i1)
    lq0 = len(quot0)
    lq1 = len(quot1)
    
    while i0 < i1:
    
        istart = string.find(text,quot0,i0,i1)
#        print("istart = %i" % istart)
        if istart > -1:
        
            iend = string.find(text,quot1,istart+lq0,i1)
#            print("iend = %i" % iend)
            
            if iend == -1:
                iend = i1
                
            tup = (istart+lq0,iend)            
            liste.append(tup)
            
            i0 = iend+lq1
        else:
        
            i0 = i1
    
    return liste

def get_index_no_quot(text,quot0,quot1):
    """ Gibt Indexpaare (Tuples) in dem der Text nicht gequotet ist z.B.
    
        text  = "abc {nest} efg  {plab}"
        #        0123456789012345678901
        quot0 = "{"
        quot1 = "}"
        
        a = get_index_quot(text,quot0,quot1)
        
        a ergibt [(0,4),(10,16)]
    """
    
    liste = []
    
    i0  = 0
    i1  = len(text)
#    print("i1 = %i" % i1)
    lq0 = len(quot0)
    lq1 = len(quot1)
    
    istart = 0
    
    while istart < i1:
    
        iend = string.find(text,quot0,istart,i1)
#        print("iend = %i" % iend)
        if iend == -1:
            iend = i1
            
        tup = (istart,iend)
        if( istart != iend ):
                liste.append(tup)
            
        i0 = string.find(text,quot1,iend+lq0,i1)
        
        if i0 == -1:
            istart = i1
        else:
            istart = i0+lq1
        
            
    return liste
def get_string_quoted(text,quot0,quot1):
    """ Gibt Liste mit string in dem der gequotet Text steht z.B.
    
        text  = "abc {nest} efg  {plab}"
        #        0123456789012345678901
        quot0 = "{"
        quot1 = "}"
        
        a = get_string_quot(text,quot0,quot1)
        
        a ergibt ["nest","plab"]
    """
    iliste = get_index_quot(text,quot0,quot1)
    liste = []
    for t in iliste:

        tdum = text[t[0]:t[1]]
        liste.append(tdum)
    return liste
def elim_comment_not_quoted(text,comment_liste,quot0,quot1):
    """ eliminiert Kommentar aus dem Text, wenn ein Kommentarzeichen
        aus der Liste comment_list im nichtgequoteten Text vorkommt
    """
    
    text1 = text
    
    a_liste = get_index_no_quot(text,quot0,quot1)
    i0 = len(text1)
#    print("i0= %i" % i0)
         
#    print(a_liste
    for a in a_liste:
    
        for comment in comment_liste:
#            print("a[0]= %i" % a[0])
#            print("a[1]= %i" % a[1])
            b = string.find(text1,comment,a[0],a[1])
#            print("b= %i" % b)
            if b > -1:
                i0 = min(b,i0)
#                print("i0= %i" % i0)

    return text1[0:i0]
        
def such(text,muster,regel="vs"):
    """
    Suche nach dem Muster in dem Text nach der Regel:

    Input:
    text    string  zu durchsuchender String
    muster  string  muster nachdem gesucht wird
    regel   string  Regel nach der gesucht wird:
                    vs  vorwaerts muster suchen
                    vn  vorwaerts suchen, wann muster nicht mehr
                        vorhanden ist
                    rs  rueckwrts muster suchen
                    rn  rueckwaerts suchen, wann muster nicht mehr
                        vorhanden ist

    Output:
    index   int     Index im String in der die Regel wahr geworden ist
                    oder -1 wenn die Regel nicht wahr geworden ist
    """
    lt = len(text)
    lm = len(muster)

    if (string.find(regel,"v") > -1) or (string.find(regel,"V") > -1):
        v_flag = 1
    else:
        v_flag = 0

    if (string.find(regel,"n") > -1) or (string.find(regel,"n") > -1):
        n_flag = 1
    else:
        n_flag = 0

    if v_flag == 1:  # vorwaerts

        if n_flag == 1: # nicht mehr vorkommen

            ireturn = -1
            for i in range(0,lt,lm):

                if( string.find(text[i:lt],muster) != 0 ):
                    ireturn = i
                    break
                #endif
            #endof
            return ireturn

        else: #soll vorkommen

            ireturn = string.find(text,muster)
            # print "ireturn = %i" % ireturn
            return ireturn
        #endif
    else: # rueckwaerts

        if n_flag == 1:

            ireturn = -1
#           print "lm=%i" % lm
#           print "lt=%i" % lt
            for i in range(0,lt,lm):

#               print "i=%i" % i
                i0 = string.rfind(text,muster,0,lt-i)
#               print "i0=%i" % i0
#               print "lt-lm-i=%i" % (lt-lm-i)

                if i0 < lt-lm-i:
                    ireturn = lt-i-1
                    break
            return ireturn

        else:

            ireturn = string.rfind(text,muster)
            return ireturn
    
def elim_a(text,muster):
    """ Schneidet muster am Anfang weg
    """
    
    i0 = such(text,muster,"vn")
#    print("i0=%i" % i0)
    
    text = text[i0:len(text)]

    return text

def elim_e(text,muster):
    """ Schneidet muster am Anfang weg
    """
    
    i0 = such(text,muster,"rn")
#    print("i0=%i" % i0)
    
    text = text[0:i0+1]

    return text
    
def elim_ae(text,muster):
    """ Schneidet muster am Anfang und Ende weg
    """
    
    text = elim_a(text,muster)
    text = elim_e(text,muster)
    
    return text                
            
def split_not_quoted(text,split,quot0,quot1,elim_leer=0):

    ls     = len(split)
    i_list = get_index_no_quot(text,quot0,quot1)

    index = []
    for i in i_list:
        flag = 1
        i0 = i[0]
        while flag == 1:
            flag = 0
            a = string.find(text,split,i0,i[1])
            if( a > -1 ):
                flag = 1
                index.append(a)
                i0 = a + ls

    
    liste = []
    i0    = 0
    for i in index:

        liste.append(text[i0:i])

        i0 = i + ls

    liste.append(text[i0:len(text)])

    if( elim_leer ):

        liste1 = []
        for t in liste:
            if( len(t) > 0 ):
                liste1.append(t)
        liste = liste1
        

    return liste

def split_with_quot(text,quot0,quot1):

    
    i_list = get_index_no_quot(text,quot0,quot1)
    print("no_quot")
    print(i_list)

    liste = []
    # kein nicht quotierter Text
    if( len(i_list) == 0 ):
        liste.append([text,QUOT]) #alles im quot
    else:

        i     = 0
        ilast = 0
        for il in i_list:
            i0 = il[0]
            i1 = il[1]
            if( i == 0 ):
                if( i0 != 0 ): #als erstes kommt gequoteter Text
                    liste.append([text[0:i0],QUOT])
                liste.append([text[i0:i1],NO_QUOT])
                ilast = i1                
            else:
                liste.append([text[ilast:i0],QUOT])
                liste.append([text[i0:i1],NO_QUOT])
                ilast = i1
            i = i+1

        if( ilast+1 < len(text) ): # noch ein Rest vorhanden
            liste.append([text[ilast:],QUOT])
            
    return liste

def change(text,muster_alt,muster_neu):
    """ ersetzt einmal alle muster_alt mit muster_neu im Text
    """
    liste = text.split(muster_alt)
    n0 = len(liste)
    if( n0 <= 1 ):
        text1 = text
    else:

        text1 = ""
        for i in range(0,n0-1):
            text1=text1+liste[i]+muster_neu
        text1=text1+liste[n0-1]
    return text1

def change_max(text,muster_alt,muster_neu):
    """ ersetzt soof es geht alle muster_alt mit muster_neu im Text
    """
    while( 1 ):
        text1 = change(text,muster_alt,muster_neu)

        if( text1 == text ):
            break
        else:
            text = text1
            
    return text1       

def slice(text,l1):
    liste = []
    t  = text
    while( len(t) > l1 ):
        liste.append(t[0:l1])
    t = t[l1:]
    if( len(t) > 0 ):
        liste.append(t)
    return liste

if __name__ == '__main__':
#    a = get_index_quot("abc\"nest\"efg\"plab\"","\"","\"")
#    a = get_index_no_quot("abc nest efg plab ","{","}")
##    text = "abc\"nest\"efg\"plab\""
##    print("\"%s\""%text
##    a = get_string_quoted(text,"\"","\"")
#    a = slice("123451234512345123",5)
#
##    a = elim_comment_not_quoted("abcdef#l123ghi","#","{","}")    
##    a = such("aaaabcdefccc","a","vn")
##    a = elim_ae("bbbbxxxxxbbbb","b")
#    text = "ab   \"10 a\""
#    print("\"%s\""%text
#    a = split_not_quoted(text," ","\"","\"",1)
#    a = change_max("abc        ijk","  "," ")
    text = "\"1234\"abc\"5678\"def\"91011\"ghi"
    print("\"%s\""%text)
    a=split_with_quot(text,"\"","\"")
    print(a)
    
        
    
