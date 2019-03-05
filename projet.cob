IDENTIFICATION DIVISION.

        PROGRAM-ID. Projet. 

ENVIRONMENT DIVISION.

        INPUT-OUTPUT SECTION. 
        FILE-CONTROL.
        SELECT Femploye ASSIGN TO "employes.dat"
        ORGANIZATION INDEXED
        RECORD KEY fe_idEmploye
        FILE STATUS IS FcER.

        SELECT Fproduit ASSIGN TO "produits.dat"
        ORGANIZATION INDEXED
        RECORD KEY fp_idProduit
        FILE STATUS IS FcPR.

        SELECT Fclient ASSIGN TO "clients.dat"
        ORGANIZATION INDEXED
        RECORD KEY fc_idClient
        FILE STATUS IS FcCR.  
 
        SELECT Fachat ASSIGN TO "achats.dat"
        ORGANIZATION INDEXED
        RECORD KEY fa_idAchat
        ALTERNATE RECORD KEY fa_idProduit WITH DUPLICATES 
        ALTERNATE RECORD KEY fa_dateAch WITH DUPLICATES
        ALTERNATE RECORD KEY fa_idEmploye WITH DUPLICATES
        ALTERNATE RECORD KEY fa_idClient WITH DUPLICATES
        FILE STATUS IS FcAR. 

        SELECT Fhistorique ASSIGN TO "historiques.dat"
        ORGANIZATION INDEXED
        RECORD KEY fh_cle
        FILE STATUS IS FcHR. 

        SELECT Fidentifiant ASSIGN TO "identifiants.dat"
        ORGANIZATION SEQUENTIAL 
        FILE STATUS IS FcIR.

DATA DIVISION.

        FILE SECTION. 
        FD Femploye.
        01 employeTemp.
                02 fe_idEmploye PIC 9(4).
                02 fe_nom PIC A(30).
                02 fe_prenom PIC A(30).
                02 fe_salaire PIC 9(10).
                02 fe_RIB PIC A(36).
                02 fe_adresse PIC A(30).
                02 fe_nbVentes PIC 9(36).

        FD Fproduit.
        01 produitTemp.
                02 fp_idProduit PIC 9(4).
                02 fp_nom PIC A(30).
                02 fp_prix PIC 9(10).
                02 fp_quantite PIC 9(36).

        FD Fclient.
        01 clientTemp.
                02 fc_idClient PIC 9(4).
                02 fc_nom PIC A(30).
                02 fc_prenom PIC A(30).
                02 fc_dateNaiss.
                	03 fc_annee PIC 9(4).
                        03 fc_mois PIC 9(2).
                        03 fc_jour PIC 9(2).
                02 fc_codePost PIC 9(5).
                02 fc_nbArtAch PIC 9(36).

        FD Fachat.
        01 achatTemp.
                02 fa_idAchat PIC 9(4).
                02 fa_idProduit PIC 9(4).
                02 fa_dateAch.
                	03 fa_annee PIC 9(4).
                        03 fa_mois PIC 9(2).
                        03 fa_jour PIC 9(2).
                02 fa_idEmploye PIC 9(4).
                02 fa_idClient PIC 9(4).

        FD Fidentifiant. 
        01 identTemp.
                02 fi_idEmploye PIC 9(4).
                02 fi_idProduit PIC 9(4).
                02 fi_idClient PIC 9(4).
                02 fi_idAchat PIC 9(4).

        FD Fhistorique. 
        01 histoTemp.
                02 fh_cle.
                	03 fh_annee PIC 9(4).
                        03 fh_mois PIC 9(2).
                        03 fh_idArticle PIC 9(4).
                02 fh_nbAricleVendu PIC 9(36).
                
                
       WORKING-STORAGE SECTION.
       77 FcER PIC 9(2). 
       77 FcPR PIC 9(2).  
       77 FcCR PIC 9(2). 
       77 FcAR PIC 9(2).
       77 FcIR PIC 9(2).
       77 FcHR PIC 9(2). 
        






