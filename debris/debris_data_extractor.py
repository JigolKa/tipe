import requests
import numpy as np
import requests
import csv
from astropy import units as u 
from hapsira.bodies import Earth
from hapsira.twobody import Orbit
from hapsira.plotting import OrbitPlotter
from hapsira.plotting.orbit.backends import Plotly3D
from hapsira.maneuver import Maneuver


#------Fonction permettant de selectionner les n débris les plus coplanaires entre eux------
def select_n_coplanaire_debris(lst_debris, n):
    r = (len(lst_debris) - 1) // 2 #on prend l'indice median
    debris_de_base = lst_debris[r] #on prend comme debris de base le debris à la mediane de la liste (choix arbitraire)
    
    # Conversion en float puis en radians
    inc_base = np.radians(float(debris_de_base['INCLINATION'])) #on recupère l'inclinaison de l'orbite de ce debris
    raan_base = np.radians(float(debris_de_base['RA_OF_ASC_NODE'])) #tout comme son RAAN
    #On calcule leur cosinus
    cos_base = np.cos(inc_base)
    sin_base = np.sin(inc_base)

    
    for i in lst_debris:


        inc_i = np.radians(float(i['INCLINATION']))
        raan_i = np.radians(float(i['RA_OF_ASC_NODE']))

        #calcul du cos de l'angle d'écart angualire netre deux plans d'orbites (savoir démontrer la formule chef )
        cos_teta = (cos_base * np.cos(inc_i)) + (sin_base * np.sin(inc_i) * np.cos(raan_base - raan_i))
        
        
        cos_teta = np.clip(cos_teta, -1.0, 1.0) 
        
        # Conversion du résultat en degrés pour la condition
        teta_deg = np.degrees(np.arccos(cos_teta))
        
        # On enregsitre cet écart angulaire directement dans le dictionnaire du débris, on créant une clé ECART ANGULAIRE
        i['ECART_ANGULAIRE'] = teta_deg

    #On trie la liste selon le paramètre ECART_ANGULAIRE
    lst_triee = sorted(lst_debris, key=lambda x: x['ECART_ANGULAIRE'])
            
    return lst_triee[:n]

#------Fonction de calcul de a matrice de cout statique -----------
def matrix_costs_maker(n,liste_orbites,tab_temps):
    M = np.zeros((n, n))
    M_temps = np.zeros((n, n))
    for i in range(n):
        for j in range (n):
            #Si les deux indice sont égaux le cout d'un transfert d'un objet à soit même est nul
            if i==j :
                M[i][j] = 0.0
                continue
            dv_min = float('inf') #On définit le min de delta v à l'infini
            best_temps = 0.0

            #On parcourt la liste des temps : pour chaque temp on calule le dv entre deux orbites et on ne garde que le dv minimal et le temps associé 
            for min_t in tab_temps:
                t_min = min_t*u.min
                cible_propagee = liste_orbites[j].propagate(t_min)
                try :
                    manoeuvre = Maneuver.lambert(liste_orbites[i],cible_propagee)
                    dv = np.linalg.norm(manoeuvre.impulses[0][1].to_value(u.m/u.s)) + np.linalg.norm(manoeuvre.impulses[1][1].to_value(u.m/u.s))

                    if dv<dv_min:
                        dv_min = dv
                        best_temps = min_t

                        
                except:
                    #Si le transfert a échoué 
                    pass
            M_temps[i][j] = best_temps
            M[i][j] = dv_min
    return M,M_temps

# URL avec le format CSV
url = "https://celestrak.org/NORAD/elements/gp.php?GROUP=cosmos-2251-debris&FORMAT=csv"
reponse = requests.get(url, headers={'User-Agent': 'Mozilla/5.0'})

# Lecture des données CSV
lignes = reponse.text.strip().split('\n')
lecteur_csv = csv.DictReader(lignes)

cibles_mission = []

# On charge les débris du fichier
cibles_mission = list(lecteur_csv)
n=12

print(f"Affichage des données : {n} sélectionnés")
lst_debris_n_cibles = select_n_coplanaire_debris(cibles_mission,n)
# Affichage avec tes noms de colonnes
for cible in lst_debris_n_cibles:
    print(f"{cible['OBJECT_NAME']}")
    print(f"  - Inclinaison : {cible['INCLINATION']}°")
    print(f"  - RAAN : {cible['RA_OF_ASC_NODE']}°")
    print(f"  - Excentricité : {cible['ECCENTRICITY']}")
    print(f"  - Ecart angulaire : {cible['ECART_ANGULAIRE']}\n")

#---------Création de la liste des orbites n débris cibles -------------
lst_orb = []
for debris in lst_debris_n_cibles:
    #Le fichier celesTrack ne donne pas a (le demi grand axe) on le retrouve donc avec  la 3ème loi  de Kepler 
    n_rad_s= float(debris['MEAN_MOTION']) * 2*np.pi/(24*3600) #Conversion en rad/s
    #On sait que n désigne le nombre de tour par peridoe ici le nombre d erevolution par jour 
    # ainsi n=2*pi /T ,T la période de revolution 
    #3 eme loi de Kepler : T²/a³ = 4pi²/mu , avec mu la constante de gravitation ce qui donne a = (mu/n²)^(1/3)
    mu = Earth.k.to_value(u.m**3 / u.s**2)
    a_debris = (mu/n_rad_s**2)**(1/3)
    orb_i = Orbit.from_classical(
     Earth,
     a = a_debris*u.m,#Correspond au demi grand axe de l'orbite
     ecc = float(debris['ECCENTRICITY'])*u.one,# Correspond à l'exentricité de l'orbite
     inc = float(debris['INCLINATION'])*u.deg,# Correspond à l'inclinaison de l'orbite
     raan = float(debris['RA_OF_ASC_NODE'])*u.deg,# Ascension droite du nœud ascendant mesure l'angle de rotation du plan de l'orbite par rapport au plan géométrique de l'équateur terrstres
     argp = float(debris['ARG_OF_PERICENTER'])*u.deg,# Correspond à l'argument du périgé
     nu = float(debris['MEAN_ANOMALY'])*u.deg 
    )
    lst_orb.append(orb_i)

#-----------------Création de la matrice de cout statique------------------

t = np.arange(10, 151, 1)  

matrice1,matrice2 = matrix_costs_maker(n,lst_orb,t)
matrice_cout_statique = np.round(matrice1,3)
matrice_temps = np.round(matrice2,3)


# print(matrice_cout_statique)
# print()
# print(matrice_temps)
#--------Exportation en TXT pour la partie en OCAML -----------
np.savetxt('matrice_cout.txt', matrice_cout_statique, fmt='%.3f', delimiter=',', header=str(n), comments='')


