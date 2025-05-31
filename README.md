# lambda-interpreter
## Završni projekt iz kolegija Semantika programskih jezika 24./25.


## Kako radi

Ovaj interpreter implementira evaluaciju izraza u ne-tipiziranom lambda računu, koji je temeljni formalni model funkcijskih programa i funkcija općenito.

### 1. Parsiranje izraza

Na početku, interpreter prima ulazni izraz napisan u tekstualnom obliku koristeći standardnu lambda sintaksu.  
Primjer: `(\x. x) y`

Parser (modul `Parser.hs`) pretvara ovaj tekst u strukturirani oblik poznat kao **AST** (Abstract Syntax Tree) — stablo koje predstavlja strukturu izraza.  
AST omogućava programu da razumije koji su dijelovi izraza funkcije, koji su varijable, a koje su aplikacije.

### 2. Reprezentacija izraza (AST)

U `Syntax.hs` definiran je tip podataka koji predstavlja izraze lambda računa:

- **Varijabla** — ime varijable (npr. `x`)
- **Apstrakcija** — lambda funkcija s parametrom i tijelom (npr. `\x. e`)
- **Primjena** — primjena funkcije na argument (npr. `(f x)`)

Ova reprezentacija omogućuje jednostavno manipuliranje i transformaciju izraza tijekom evaluacije.

### 3. Evaluacija izraza (redukcija)

Evaluacija (modul `Evaluator.hs`) provodi se primjenom pravila lambda računa, ponajprije **beta-redukcije**, koja znači:

- Kada se funkcija (`\x. e`) primijeni na argument `a` (zapisa kao `( \x. e ) a`), zamijenimo svako pojavljivanje varijable `x` u izrazu `e` s argumentom `a`.
- Rezultat je novi, pojednostavljeni izraz.

Na primjer:

```haskell
(\x. x) y  →  y
```

Interpreter koristi normalni red evaluacije, što znači da se najprije evaluira vanjski, lijevi dio izraza, pa tek onda unutarnji.

Main.hs implementira interaktivnu konzolu gdje korisnik može unositi lambda izraze, a interpreter ih parsira, evaluira i ispisuje rezultat.
