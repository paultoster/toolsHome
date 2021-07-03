



// Instanz-Struktur
struct SSlfParInst {

    slf::CStr				  Name;           // Name
    SSlfParVar	    *pVar;          // Pointer zu den Variablen
    SSlfParTab1D    *pTab1D;        // eindimensionale Tabelle
    SSlfParTab2D    *pTab2D;        // eindimensionale Tabelle
    SSlfParGroup    *pGroup;        // Gruppen 1. Hierachie
    SSysParInst     *pNext;         // nächster Pointer
};
