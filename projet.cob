IDENTIFICATION DIVISION.
        PROGRAM-ID. Projet.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT Femploye ASSIGN TO "employes.dat"
           ORGANIZATION INDEXED
           RECORD KEY fe_idEmploye
           ALTERNATE RECORD KEY fe_login WITH DUPLICATES
           ALTERNATE RECORD KEY fe_mdp WITH DUPLICATES
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
                      02 fe_salaire PIC 9(5)V9(2).
                      02 fe_rib PIC A(23).
                      02 fe_adresse PIC A(30).
                      02 fe_nbVente PIC 9(13).
                      02 fe_role PIC 9(1).
                      02 fe_mdp PIC A(30).

           FD Fproduit.
              01 produitTemp.
                      02 fp_idProduit PIC 9(4).
                      02 fp_nom PIC A(30).
                      02 fp_prix PIC 9(5).
                      02 fp_quantite PIC 9(13).

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
                      02 fc_nbArtAch PIC 9(13).

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
                      02 fh_nbAricleVendu PIC 9(13).


        WORKING-STORAGE SECTION.
          77 FcER PIC 9(2).
          77 FcPR PIC 9(2).
          77 FcCR PIC 9(2).
          77 FcAR PIC 9(2).
          77 FcIR PIC 9(2).
          77 FcHR PIC 9(2).
          77 VRAI PIC X VALUE "V".
          77 FAUX PIC x VALUE "F".
          77 FIN-MENU PIC X.
          77 CHOIX-MENU PIC X.

          77 we_idEmploye PIC 9(4).
          77 we_nom PIC A(30).
          77 we_prenom PIC A(30).
          77 W-TROUVE PIC 9(1).
          77 W-FIN PIC 9(1).
          77 w_ee PIC 9(1).

          77 w_pe PIC 9(1).
          77 w_idProduit PIC 9(4).

          77 wh_annee PIC 9(4).
          77 wh_mois PIC 9(4).
          77 wh_compteur PIC 9(5).

          77 w_login PIC 9(1).

          77 wa_idAchat PIC 9(4).
          77 wa_jour PIC 9(2).
          77 wa_idClient PIC 9(4).
          77 w_ae PIC 9(1).

          77 w_ce PIC 9(1).


PROCEDURE DIVISION.
            *>ICI CORPS DU PROGRAMME
             MOVE FAUX TO FIN-MENU.
             MOVE VRAI TO ERREUR.
             PERFORM MENU UNTIL FIN-MENU = VRAI.
STOP RUN.

menu.
         IF fe_role = 1
           THEN PERFORM menu_admin
           ELSE PERFORM menu_employe
         END-IF.

menu_admin.
         DISPLAY "Quelle action souhaitez-vous faire ?"
         DISPLAY " "
         DISPLAY "A. Ajouter un client."
         DISPLAY "B. Rechercher le premier client."
         DISPLAY "C. Ajouter un employe."
         DISPLAY "D. Enregistrer les ventes."
         DISPLAY "E. Ajouter un produit."
         DISPLAY "F. Rechercher un produit."
         DISPLAY "G. Calculer le pourcentage d'un produit."
         DISPLAY "H. Ajout d'un achat."
         DISPLAY "I. Calculer le meilleur employe."
         DISPLAY "J. Quitter."
         ACCEPT CHOIX-MENU
         EVALUATE CHOIX-MENU
         WHEN "A"
                PERFORM AJOUT_CLIENT
         WHEN "B"
                PERFORM RECHERCHE_PREMIER_CLIENT
         WHEN "J"
                MOVE VRAI TO FIN-MENU
         WHEN OTHER
                DISPLAY "choix non valide."
         END-EVALUATE.

menu_employe.
         DISPLAY "Quelle action souhaitez-vous faire ?"
         DISPLAY " "
         DISPLAY "A. Ajouter un client."
         DISPLAY "B. Rechercher le premier client."
         DISPLAY "C. Enregistrer les ventes."
         DISPLAY "D. Ajouter un produit."
         DISPLAY "E. Rechercher un produit."
         DISPLAY "F. Quitter."
         ACCEPT CHOIX-MENU
         EVALUATE CHOIX-MENU
         WHEN "A"
                PERFORM AJOUT_CLIENT
         WHEN "B"
                PERFORM RECHERCHE_PREMIER_CLIENT
         WHEN "J"
                MOVE VRAI TO FIN-MENU
         WHEN OTHER
                DISPLAY "choix non valide."
         END-EVALUATE.

login.
         OPEN INPUT Femploye
            PERFORM WITH TEST AFTER UNTIL w_login > 0
                    PERFORM WITH TEST AFTER UNTIL FcER
                       DISPLAY "Saisir votre id : "
                       ACCEPT fe_idEmploye
                       DISPLAY "Saisir le mot de passe : "
                       ACCEPT fe_mdp
                    END-PERFORM
                    READ Femploye
                       INVALID KEY
                       DISPLAY "Connection refusé !"
                       MOVE 0 TO w_login
                       NOT INVALID KEY
                       DISPLAY "Connection réussie !"
                       MOVE 1 TO w_login
                    END-READ
            END-PERFORM
         CLOSE Femploye.

recherche_produit.
         OPEN INPUT Fproduit
            READ Fproduit
               INVALID KEY MOVE 0 TO w_pe
               NOT INVALID KEY MOVE 1 TO w_pe
            END-READ
         CLOSE Fproduit.

recherche_employe.
         OPEN INPUT Femploye
            READ Femploye
               INVALID KEY MOVE 0 TO w_ee
               NOT INVALID KEY MOVE 1 TO w_ee
            END-READ
         CLOSE Femploye.

recherche_client.
         OPEN INPUT Fclient
            READ Fclient
               INVALID KEY MOVE 0 TO w_ce
               NOT INVALID KEY MOVE 1 TO w_ce
            END-READ
         CLOSE Fclient.

recherche_achat.
         OPEN INPUT Fachat
            READ Fachat
               INVALID KEY MOVE 0 TO w_ae
               NOT INVALID KEY MOVE 1 TO w_ae
            END-READ
         CLOSE Fachat.

ajout_produit.
         OPEN EXTEND Fproduit
            PERFORM WITH TEST AFTER UNTIL fp_idProduit > 0 AND w_pe = 0
               DISPLAY "Saisir l'identifiant du produit : "
               ACCEPT fp_idProduit
               PERFORM recherche_produit
            END-PERFORM
            DISPLAY "Saisir le nom du produit : "
            ACCEPT fp_nom
            PERFORM WITH TEST AFTER UNTIL fp_prix > 0
               DISPLAY "Saisir le prix du produit : "
               ACCEPT fp_prix
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL fp_quantite > 0
               DISPLAY "Saisir la quantite du produit : "
               ACCEPT fp_quantite
            END-PERFORM
            MOVE w_idProduit TO fp_idProduit
            WRITE produitTemp
         CLOSE Fproduit.

ajout_employe.
         OPEN I-O Femploye
            PERFORM WITH TEST AFTER UNTIL W-TROUVE = 0
               DISPLAY "Entrez identifiant : "
               ACCEPT we_idEmploye
               DISPLAY "Entrez le nom : "
               ACCEPT we_nom
               DISPLAY "Entrez le prénom : "
               ACCEPT we_prenom
               MOVE 0 TO W-FIN
               MOVE 0 TO W-TROUVE
               PERFORM WITH TEST AFTER UNTIL W-TROUVE = 1 OR W-FIN = 1
                  READ Femploye NEXT
                  AT END
                     MOVE 1 TO W-FIN
                  NOT AT END
                     IF we_idEmploye = fe_idEmploye AND
                                       we_nom = fe_nom AND
                                             we_prenom = fe_prenom THEN
                        MOVE 1 TO W-TROUVE
                     END-IF
                  END-READ
               END-PERFORM
             END-PERFORM
             PERFORM WITH TEST AFTER UNTIL fe_salaire<55 AND
                                                   fe_salaire>1171.34
               DISPLAY "Entrez le salaire : "
               ACCEPT fe_salaire
             END-PERFORM
             DISPLAY "Entrez le RIB : "
             ACCEPT fe_rib
             DISPLAY "Entrez l'adresse : "
             ACCEPT fe_adresse
             PERFORM WITH TEST AFTER UNTIL fe_nbVente>0
               DISPLAY "Entrez le nombre de vente : "
               ACCEPT fe_nbVente
            END-PERFORM
            WRITE employeTemp
         CLOSE Femploye.

enregistre_historique.
         OPEN INPUT Fachat
            DISPLAY "Entrez l'annee : "
            ACCEPT wh_annee
            DISPLAY "Entrez le mois : "
            ACCEPT wh_mois
            Move 1 TO fa_idProduit
            START Fachat, KEY IS = fa_idProduit
            INVALID KEY
               DISPLAY 'FIN'
            NOT INVALID KEY
            MOVE 1 TO w_idProduit
            MOVE 0 TO wh_compteur
               PERFORM WITH TEST AFTER UNTIL W-FIN = 1
                  READ Fachat NEXT
                  AT END
                     MOVE 1 TO W-FIN
                  NOT AT END
                     IF w_idProduit = fa_idProduit THEN
                        ADD 1 TO wh_compteur
                     ELSE
                        MOVE fa_idProduit TO w_idProduit
                        OPEN EXTEND Fhistorique
                           WRITE histoTemp
                        CLOSE Fhistorique
                        MOVE 1 TO wh_compteur
                     END-IF
                  END-READ
               END-PERFORM
            END-START
         CLOSE Fachat.

ajout_achat.
         OPEN I-O Fachat
            PERFORM WITH TEST AFTER UNTIL fa_idAchat > 0 AND w_ae = 1
               DISPLAY "Saisir l'identifiant de l'achat : "
               ACCEPT wa_idAchat
               PERFORM recherche_achat
            END-PERFORM

            PERFORM WITH TEST AFTER UNTIL fp_idProduit > 0 AND w_pe = 1
               DISPLAY "Saisir l'identifiant du produit : "
               ACCEPT fp_idProduit
               PERFORM recherche_produit
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL wh_annee>2000 AND wh_mois>0
                                          AND wh_mois<13 AND wa_jour>0
                                                AND wa_jour<32
               DISPLAY "Saisir l'annee d'achat : "
               ACCEPT wh_annee
               DISPLAY "Saisir le mois d'achat : "
               ACCEPT wh_mois
               DISPLAY "Saisir le jour d'achat : "
               ACCEPT wa_jour
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL fe_idEmploye > 0 AND w_ee = 1
               DISPLAY "Saisir l'id employe : "
               ACCEPT fe_idEmploye
               PERFORM recherche_employe
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL fc_idClient > 0 AND w_ce = 1
               DISPLAY "Saisir l'id du client : "
               ACCEPT fc_idClient
               PERFORM recherche_client
            END-PERFORM
            WRITE achatTemp
         CLOSE Fachat.
