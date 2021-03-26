**[4] Implémentation d'un serveur HTTP**

Défi sur une semaine, à rendre au plus tard le lundi 5 avril à 6h à votre ambassadeur préféré. Ce défi est considéré plus difficile que d'ordinaire, vous avez une semaine pour le résoudre.

**Le serveur**

Le serveur tourne en HTTP/1.0 derrière le port 8080 et est compatible avec les principaux navigateurs. Le serveur comprend les requêtes `HEAD`, `GET`, `POST`, `PUT` et `DELETE`. Le serveur expose une application "compteur" à l'url <http://compteur.notaname.fr>, les autres url *doivent* renvoyer une page d'erreur au format HTML avec un code 404. Le serveur *doit* être capable de répondre à plusieurs utilisateurs à la fois. Le serveur *peut* être accessible à l'extérieur de votre réseau domestique. Le serveur *peut* comprendre TLS, dans quel cas il écoute sur le port 8443.

Parce que le but de ce défi est d'implémenter un serveur web, il est bien entendu interdit d'utiliser un quelconque framework. Plus précisement, vous n'avez pas le droit d'utiliser une quelconque bibliothèque HTTP et vous vous contentez des bibliothèques réseaux et systèmes de votre langage. Le choix de votre stack système est libre, vous pouvez faire du multithreading, de l'asynchrone, etc.

**L'application**

L'application permet de manipuler des compteurs. Le serveur démarre avec deux compteurs : "carotte" et "etoile". Le compteur "carotte" est normal et est initialisé à 10. Un compteur "etoile" est le seul compteur spécial, il n'est pas possible de l'incrémenter et il renvoie la somme de tous les autres compteurs. La valeur d'un compteur ne peut que croitre.

L'application compteur admet 5 routes (en plus de `HEAD`) :

* `GET /` : retourne la liste de tous les compteurs.
* `GET /<compteur>` : retourne l'état du compteur `<compteur>`.
* `POST /` : crée un nouveau compteur et le renvoie.
* `PUT /<compteur>` : met à jour la valeur du compteur `<compteur>`.
* `DELETE /<compteur>` : supprime le compteur `<compteur>`.

Il est possible de communiquer en `x-www-form-urlencoded` et en `JSON` avec l'application (requête). L'application est capable de répondre au format `HTML`, au format `JSON` ou à défaut au format texte (réponse). Interessez vous aux headers `Accept` et `Content-Type`.

Pour simuler une application plus compliquée et mettre à mal votre implémentation réseau, chaque route simule des calculs et des requêtes à un service externe. Vous simulez les calculs en faisant tourner une boucle dans le vide à raison de 200 millisecondes. Vous simulez une requête externe en mettant votre processus en *sleep* pendant 300 millisecondes au milieu.

Exemple en Python, après des tests vous avez trouvé qu'itérer sur 1 millard d'éléments prend 200 millisecondes.

```python
counters = {'carotte': 10}

def get(counter):
    for i in range(1_000_000_000): pass
    time.sleep(0.3)  # remplacez par asyncio.sleep(0.3) si vous faites de l'async
    
    return b"{'name': 'carotte', 'value': %d}" % counters[counter]
```

**Considérations techniques**

Pour faciliter l'implémentation, vous pouvez limiter la taille du head à maximum 1024 caractères par ligne. La RFC (de ce qu'on sait) ne précise rien à ce propos et la plupart des serveurs permettent 8k ou 16k caractères par ligne dans les headers.

**Ressources**

Voici une liste de ressource que nous vous conseillons de lire. Nous ajouterons à cette liste toutes les ressources que vous nous conseillerez.

* <https://fr.wikipedia.org/wiki/Hypertext_Transfer_Protocol>
* <https://www.pierre-giraud.com/http-reseau-securite-cours/>
* <http://www.kegel.com/c10k.html>
* <https://www.w3.org/Protocols/HTTP/1.0/spec.html>
