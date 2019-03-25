IDENTIFICATION DIVISION.
        PROGRAM-ID. Projet.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT Femploye ASSIGN TO "employes.dat"
           ORGANIZATION INDEXED
	  	   ACCESS MODE DYNAMIC
           RECORD KEY fe_idEmploye
           FILE STATUS IS FcER.
		   
           SELECT Fproduit ASSIGN TO "produits.dat"
           ORGANIZATION INDEXED
	  	   ACCESS MODE DYNAMIC
           RECORD KEY fp_idProduit
           FILE STATUS IS FcPR.
		   
           SELECT Fclient ASSIGN TO "clients.dat"
           ORGANIZATION INDEXED
	       ACCESS MODE DYNAMIC
           RECORD KEY fc_idClient
           FILE STATUS IS FcCR.
		   
           SELECT Fachat ASSIGN TO "achats.dat"
           ORGANIZATION INDEXED
	       ACCESS MODE DYNAMIC
           RECORD KEY fa_idAchat
           ALTERNATE RECORD KEY fa_idProduit WITH DUPLICATES
           ALTERNATE RECORD KEY fa_dateAch WITH DUPLICATES
           ALTERNATE RECORD KEY fa_idEmploye WITH DUPLICATES
           ALTERNATE RECORD KEY fa_idClient WITH DUPLICATES
           FILE STATUS IS FcAR.
		   
           SELECT Fhistorique ASSIGN TO "historiques.dat"
           ORGANIZATION INDEXED
	       ACCESS MODE DYNAMIC
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
                  01 ws_employeTemp.
                      02 ws_idEmploye PIC 9(4).
                      02 ws_nom PIC A(30).
                      02 ws_prenom PIC A(30).
                      02 ws_salaire PIC 9(5)V9(2).
                      02 ws_rib PIC A(23).
                      02 ws_adresse PIC A(30).
                      02 ws_nbVente PIC 9(13).
                      02 ws_role PIC 9(1).
                      02 ws_mdp PIC A(30).

         77 ERREUR PIC X.
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
          77 nb_client PIC 9(4).
          77 nb_employe PIC 9(4).
          77 nb_produit PIC 9(4).
          77 nb_achat PIC 9(4).
          77 annee PIC 9(4).
          77 mois PIC 9(2).
          77 jour PIC 9(2).
          77 MAX PIC 9(13).
          77 admin PIC 9.
          77 mdp PIC X(12).
          77 EndOfFile PIC 9.
	  	  77 pourcentage PIC 9.
	  	  77 nb_ventes_totales PIC 9.
	      77 nb_produit_vendu PIC 9.
	      77 identifiant PIC 9.
	  



PROCEDURE DIVISION.
          
                MOVE FAUX TO FIN-MENU.
                perform login.
                PERFORM MENU UNTIL FIN-MENU = VRAI.
STOP RUN.

         menu.
         IF admin = 1
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
         DISPLAY "G. Calculer le pourcentage d un produit."
         DISPLAY "H. Ajout d un achat."
         DISPLAY "Q. Quitter."
         ACCEPT CHOIX-MENU
         EVALUATE CHOIX-MENU
         WHEN "A"
                PERFORM AJOUT_CLIENT
         WHEN "B"
                PERFORM RECHERCHE_PREMIER_CLIENT
         WHEN "C"
               PERFORM AJOUT_EMPLOYE
          WHEN "D"
	       PERFORM enregistre_historique
         WHEN "E"
               PERFORM AJOUT_PRODUIT
         WHEN "F"
               PERFORM recherche_produit
         WHEN "G"
	       PERFORM CALCUL_POURCENTAGE_PRODUIT
         WHEN "H"
               PERFORM ajout_achat
         WHEN "Q"
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
         DISPLAY "Q. Quitter."
         ACCEPT CHOIX-MENU
         EVALUATE CHOIX-MENU
         WHEN "A"
                PERFORM AJOUT_CLIENT
         WHEN "B"
                PERFORM RECHERCHE_PREMIER_CLIENT
         WHEN "D"
               PERFORM AJOUT_PRODUIT
	  	 WHEN "E"
	  		   PERFORM recherche_produit
         WHEN "Q"
                MOVE VRAI TO FIN-MENU
         WHEN OTHER
                DISPLAY "choix non valide."
         END-EVALUATE.
	  
        AJOUT_CLIENT.
                MOVE FAUX TO ERREUR.
                OPEN I-O Fclient.
	  		    COMPUTE nb_client = nb_client + 1
                MOVE nb_client TO fc_idClient
                DISPLAY "Entrez le nom"
                ACCEPT fc_nom
                DISPLAY "Entrez le prenom"
                ACCEPT fc_prenom
                PERFORM WITH TEST AFTER UNTIL ERREUR = FAUX
                        DISPLAY "Entrez l année de naissance"
                        ACCEPT fc_annee
                        DISPLAY "Entrez le mois de naissance"
                        ACCEPT fc_mois
                        DISPLAY "Entrez le jour"
                        ACCEPT fc_jour
                        MOVE fc_annee TO annee
                        MOVE fc_mois TO mois
                        MOVE fc_jour TO jour
                        PERFORM VERIF_DATE
                END-PERFORM
                DISPLAY "Entrez le code postal"
                ACCEPT fc_codePost
	  	        MOVE 0 TO fc_nbArtAch
                WRITE clientTemp END-WRITE
                CLOSE Fclient
	  			PERFORM MISE_A_JOUR_IDENTIFIANT.
	  
MISE_A_JOUR_IDENTIFIANT.
	  			OPEN INPUT Fidentifiant
	  			EVALUATE FcIR
	  				WHEN 35 DISPLAY "FICHIER Fidentifiant INEXISTANT"
	  	  			WHEN 00	DISPLAY "FICHIER Fidentifiant TROUVE"
	  			END-EVALUATE
	  			perform compter_tout
	  			CLOSE Fidentifiant

	  			OPEN OUTPUT Fidentifiant
	  			WRITE identTemp
	  	  		CLOSE Fidentifiant.

compter_tout.
		perform compter_employes
		MOVE nb_employe TO fi_idEmploye
		perform compter_clients
		MOVE nb_client TO fi_idClient
		perform compter_produits
		MOVE nb_produit TO fi_idProduit
		perform compter_achats
		MOVE nb_achat   TO fi_idAchat.
	  
	  
compter_clients.
      OPEN INPUT Fclient
	  EVALUATE FcCR
	  	WHEN 35 DISPLAY "FICHIER Fclient INEXISTANT"
	  	WHEN 00	DISPLAY "FICHIER Fclient TROUVE"
						MOVE 0 TO EndOfFile, nb_client
						PERFORM VARYING nb_client FROM 1 By 1 UNTIL EndOfFile = 1
								READ Fclient NEXT
										AT END MOVE 1 TO EndOfFile
								END-READ
								display "Client "  nb_client " = " clientTemp
						END-PERFORM
						SUBTRACT 2 FROM nb_client 
						DISPLAY "Il y a " nb_client " clients "
	END-EVALUATE						
	CLOSE Fclient.


compter_employes.
     OPEN INPUT Femploye
	 EVALUATE FcER
	  	WHEN 35 DISPLAY "FICHIER Femploye INEXISTANT"
	  	WHEN 00	DISPLAY "FICHIER Femploye TROUVE"
						MOVE 0 TO EndOfFile, nb_employe
						PERFORM VARYING nb_employe FROM 1 By 1 UNTIL EndOfFile = 1
								READ Femploye NEXT
										AT END MOVE 1 TO EndOfFile
										NOT AT END DISPLAY "employeTemp" employeTemp
								END-READ
						END-PERFORM
						SUBTRACT 2 FROM nb_employe 
						DISPLAY "Il y a " nb_employe " employes "
	END-EVALUATE
	CLOSE Femploye.
	  
compter_produits.
      OPEN INPUT Fproduit
	  EVALUATE FcPR
	  	WHEN 35 DISPLAY "FICHIER Fproduit INEXISTANT"
	  	WHEN 00	DISPLAY "FICHIER Fproduit TROUVE"
						MOVE 0 TO EndOfFile, nb_produit
						PERFORM VARYING nb_produit FROM 1 By 1 UNTIL EndOfFile = 1
								READ Fproduit NEXT
										AT END MOVE 1 TO EndOfFile
								END-READ
						END-PERFORM
						SUBTRACT 2 FROM nb_produit 
						DISPLAY "Il y a " nb_produit " produits "
	END-EVALUATE						
	CLOSE Fproduit.
	  
compter_achats. 
      OPEN INPUT Fachat
	  EVALUATE FcAR
	  	WHEN 35 DISPLAY "FICHIER Fachat INEXISTANT"
	  	WHEN 00	DISPLAY "FICHIER Fachat TROUVE"
						MOVE 0 TO EndOfFile, nb_achat
						PERFORM VARYING nb_achat FROM 1 By 1 UNTIL EndOfFile = 1
								READ Fachat NEXT
										AT END MOVE 1 TO EndOfFile
								END-READ
						END-PERFORM
						SUBTRACT 2 FROM nb_achat 
						DISPLAY "Il y a " nb_achat " achats "
	END-EVALUATE												
	CLOSE Fachat.
	  
RECHERCHE_PREMIER_CLIENT.
                MOVE 0 TO MAX
                OPEN INPUT Fclient
                READ Fclient NEXT
                AT END
                        DISPLAY "Le client qui a realise le plus d achat est " fc_idClient " - " fc_nom  " - " fc_prenom
				NOT AT END 
				IF fc_nbArtAch > MAX 
				THEN 
					MOVE fc_nbArtAch TO MAX
                END-READ
                CLOSE Fclient.

VERIF_DATE.
                MOVE FAUX TO ERREUR 
                IF mois > 12 
                       THEN DISPLAY "date invalide"
                       MOVE VRAI TO ERREUR
                       ELSE 
                      IF jour < 01 and jour > 31
                      THEN DISPLAY "date invalide"
                        MOVE VRAI TO ERREUR
                      END-IF   
                 END-IF.
                
	  
login.
   OPEN INPUT Femploye
    EVALUATE FcER
		WHEN 35 DISPLAY "FICHIER Femploye INEXISTANT vous etes admin !"
						MOVE 1 TO admin
	  	WHEN 00	DISPLAY "FICHIER Femploye TROUVE"
						DISPLAY "Saisir votre id : "
						ACCEPT identifiant
						DISPLAY "Saisir le mot de passe : "
						ACCEPT mdp
	  					MOVE identifiant TO fe_idEmploye
	  					READ Femploye
						INVALID KEY 
	  						DISPLAY "erreur"
	  					NOT INVALID KEY
							IF fe_mdp = mdp
						THEN 
							DISPLAY "Connection réussie !"
							MOVE fe_role TO admin
						 ELSE
							DISPLAY "Connection échouée !"
							  MOVE VRAI TO FIN-MENU
						END-IF
	  					END-READ
						CLOSE Femploye
	END-EVALUATE.	  
	  
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


AJOUT_PRODUIT.
                OPEN I-O Fproduit.
	  		    COMPUTE nb_produit = nb_produit + 1
                MOVE nb_produit TO fp_idProduit

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
            WRITE produitTemp
         CLOSE Fproduit
		 PERFORM MISE_A_JOUR_IDENTIFIANT.

AJOUT_EMPLOYE.
                OPEN INPUT Femploye
				EVALUATE FcER
				WHEN 35 DISPLAY "FICHIER Femploye INEXISTANT"
							MOVE 0 To nb_employe
				WHEN 00	DISPLAY "FICHIER Femploye TROUVE"
							MOVE 0 TO EndOfFile, nb_employe
							PERFORM VARYING nb_employe FROM 1 By 1 UNTIL EndOfFile = 1
										READ Femploye NEXT
											  AT END MOVE 1 TO EndOfFile
											  NOT AT END DISPLAY "employeTemp" employeTemp
										END-READ
							END-PERFORM
							SUBTRACT 2 FROM nb_employe 
							DISPLAY "Il y a " nb_employe " employes "
				END-EVALUATE
				CLOSE Femploye
				
				OPEN I-O Femploye
	  		    COMPUTE nb_employe = nb_employe + 1
				DISPLAY "AJOUT nb_employe" nb_employe
                MOVE nb_employe TO ws_idEmploye, fe_idEmploye
                DISPLAY "Entrez le nom"
                ACCEPT ws_nom
                DISPLAY "Entrez le prenom"
                ACCEPT ws_prenom
                DISPLAY "Entrez le salaire"
                ACCEPT ws_salaire
                DISPLAY "Entrez le RIB"
                ACCEPT ws_RIB
                DISPLAY "Entrez l adresse"
                ACCEPT ws_adresse
                DISPLAY "Entrez le role (0-User 1-Admin)"
                ACCEPT ws_role
                DISPLAY "Entrez le mot de passe" 
                ACCEPT ws_mdp
	  			MOVE 0 TO ws_nbVente
				READ Femploye
	  			INVALID KEY 
	  				DISPLAY "Erreur"
	  			NOT INVALID KEY
               		WRITE employeTemp FROM ws_employeTemp  
                END-WRITE
				DISPLAY "Création employeTemp" employeTemp
                CLOSE Femploye
		PERFORM MISE_A_JOUR_IDENTIFIANT.					
	  
enregistre_historique.
         OPEN INPUT Fachat
            DISPLAY "Entrez l annee : "
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
                        OPEN I-O Fhistorique
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
            COMPUTE nb_achat = nb_achat + 1
             MOVE nb_achat TO fa_idAchat  
            PERFORM WITH TEST AFTER UNTIL fp_idProduit > 0 AND w_pe = 1
               DISPLAY "Saisir l identifiant du produit : "
               ACCEPT fp_idProduit
               PERFORM recherche_produit
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL ERREUR = FAUX
               DISPLAY "Saisir l annee d achat : "
               ACCEPT wh_annee
               DISPLAY "Saisir le mois d achat : "
               ACCEPT wh_mois
               DISPLAY "Saisir le jour d achat : "
               ACCEPT wa_jour
			   PERFORM VERIF_DATE
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL fe_idEmploye > 0 AND w_ee = 1
               DISPLAY "Saisir l id employe : "
               ACCEPT fe_idEmploye
               PERFORM recherche_employe
            END-PERFORM
            PERFORM WITH TEST AFTER UNTIL fc_idClient > 0 AND w_ce = 1
               DISPLAY "Saisir l id du client : "
               ACCEPT fc_idClient
               PERFORM recherche_client
            END-PERFORM
            WRITE achatTemp
        CLOSE Fachat.
	  
        CALCUL_POURCENTAGE_PRODUIT.
                DISPLAY "Donnez l identifiant d un produit"
                ACCEPT identifiant
                DISPLAY "Donnez le mois"
                ACCEPT mois
                DISPLAY "Donnez l année"
                ACCEPT annee
                OPEN INPUT Fhistorique
                MOVE identifiant TO fh_idArticle
                MOVE mois TO fh_mois
                MOVE annee TO fh_annee
                READ Fhistorique
                INVALID KEY 
                	DISPLAY " le produit n existe pas " 
                NOT INVALID KEY
                        MOVE fh_nbAricleVendu TO nb_produit_vendu
                END-READ
	  			OPEN INPUT Fhistorique
	            MOVE 0 TO nb_ventes_totales
	            READ Fhistorique NEXT
	            AT END
	            	MULTIPLY 100 BY nb_produit_vendu GIVING pourcentage
                	DIVIDE pourcentage BY nb_ventes_totales GIVING pourcentage
	            	DISPLAY " pourcentage :" pourcentage " % "
	            NOT AT END
	            	IF fh_mois = mois and fh_annee = annee
	            	THEN 
	            	COMPUTE nb_ventes_totales = nb_ventes_totales + fh_nbAricleVendu
	            	END-IF
	  			END-READ
	            CLOSE Fhistorique.	  

