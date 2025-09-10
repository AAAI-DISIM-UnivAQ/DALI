# Posizionamento a Pila delle Finestre Terminali DALI

## Descrizione

Sono state implementate modifiche ai script di avvio del sistema multi-agente DALI per disporre le finestre dei terminali dei processi Sicstus Prolog in modalità sovrapposte a pila (cascading/stacked windows).

## File Modificati

1. `startmas.sh` - Script principale per l'avvio del MAS
2. `startmas_modular.sh` - Script per l'avvio del MAS con architettura modulare
3. `test_window_positioning.sh` - Script di test per verificare il posizionamento
4. `cleanup_dali_windows.sh` - Script di utilità per chiudere finestre DALI rimaste aperte
5. `example_custom_config.sh` - Esempi di configurazioni personalizzate

## Modifiche Implementate

### Costanti di Configurazione (modificabili all'inizio degli script)
```bash
# Posizionamento iniziale delle finestre
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50

# Dimensioni finestre
# macOS: dimensioni in pixel (larghezza x altezza)
WINDOW_WIDTH_MACOS=400
WINDOW_HEIGHT_MACOS=300

# Linux: dimensioni in caratteri (colonne x righe)
WINDOW_COLS_LINUX=40
WINDOW_ROWS_LINUX=12
```

### Variabili di Posizionamento (inizializzate dalle costanti)
- `window_x_pos=$WINDOW_START_X` - Posizione X iniziale della prima finestra
- `window_y_pos=$WINDOW_START_Y` - Posizione Y iniziale della prima finestra  
- `window_offset=$WINDOW_OFFSET` - Offset per il posizionamento a cascata

### Funzione `open_terminal()` Aggiornata

#### Per macOS (Darwin)
- Utilizza AppleScript con `osascript` per posizionare le finestre del Terminal
- Imposta le coordinate e dimensioni con `set bounds`
- Dimensioni finestra configurabili: 400x300 pixel (default, ridotte del 50%)

```bash
osascript -e "
tell application \"Terminal\"
    do script \"cd '$current_dir' && $cmd\"
    delay 0.5
    set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
end tell
"
```

#### Per Linux
- Supporta `gnome-terminal`, `xterm`, e `konsole`
- Utilizza il parametro `--geometry` per impostare posizione e dimensioni
- Formato geometria configurabile: `40x12+X+Y` (40 colonne, 12 righe, posizione X,Y - ridotte del 50%)

```bash
gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos
xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos
konsole --title="$title" --geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos
```

### Effetto Cascata
Dopo ogni apertura di finestra, le coordinate vengono incrementate:
```bash
window_x_pos=$((window_x_pos + window_offset))
window_y_pos=$((window_y_pos + window_offset))
```

Questo crea un effetto a cascata dove ogni nuova finestra appare spostata di 50 pixel in basso e a destra rispetto alla precedente.

## Ordine di Apertura delle Finestre

1. **DALI Server** - Prima finestra (posizione 100,100)
2. **DALI Agent 1** - Seconda finestra (posizione 150,150)
3. **DALI Agent 2** - Terza finestra (posizione 200,200)
4. **DALI Agent N** - Finestre successive (posizione incrementale)
5. **DALI User Interface** - Ultima finestra

## Test delle Modifiche

Eseguire il script di test per verificare il corretto funzionamento:

```bash
cd Examples/advanced
./test_window_positioning.sh
```

Il test aprirà 4 finestre simulate disposte a cascata per verificare il posizionamento.

## Script di Utilità

### Cleanup Finestre Rimaste Aperte
Se dovessero rimanere finestre DALI aperte dopo un arresto anomalo:

```bash
cd Examples/advanced
./cleanup_dali_windows.sh
```

Questo script:
- Su macOS: Cerca e chiude selettivamente le finestre con marker `[DALI]`
- Su Linux: Guida per terminare processi SICStus rimasti attivi

### Esempi di Configurazione
Per vedere esempi di configurazioni personalizzate:

```bash
cd Examples/advanced
./example_custom_config.sh
```

## Configurazione Personalizzata

È possibile personalizzare facilmente il posizionamento e le dimensioni modificando le costanti all'inizio degli script:

### Posizionamento
- `WINDOW_START_X` - Posizione X iniziale (default: 100)
- `WINDOW_START_Y` - Posizione Y iniziale (default: 100)  
- `WINDOW_OFFSET` - Distanza tra finestre per effetto cascata (default: 50)

### Dimensioni macOS (in pixel)
- `WINDOW_WIDTH_MACOS` - Larghezza finestra (default: 400, ridotta del 50%)
- `WINDOW_HEIGHT_MACOS` - Altezza finestra (default: 300, ridotta del 50%)

### Dimensioni Linux (in caratteri)
- `WINDOW_COLS_LINUX` - Numero di colonne (default: 40, ridotte del 50%)
- `WINDOW_ROWS_LINUX` - Numero di righe (default: 12, ridotte del 50%)

**Esempio di personalizzazione:**
```bash
# Per finestre più grandi
WINDOW_WIDTH_MACOS=600
WINDOW_HEIGHT_MACOS=450
WINDOW_COLS_LINUX=60
WINDOW_ROWS_LINUX=18

# Per maggiore spaziatura tra finestre
WINDOW_OFFSET=80
```

## Compatibilità

### macOS
- Richiede il Terminal.app standard di macOS
- Utilizza AppleScript per il controllo delle finestre
- Testato su macOS con SICStus Prolog

### Linux
- Supporta i principali emulatori di terminale
- `gnome-terminal` (raccomandato)
- `xterm` (alternativa)
- `konsole` (per desktop KDE)

## Gestione Resume macOS

**Problema Risolto**: Su macOS, Terminal.app salva automaticamente tutte le finestre aperte e le ripristina al prossimo avvio (funzionalità "Resume").

**Soluzione Implementata**:
- Ogni finestra DALI viene etichettata con un marker `[DALI]` nel titolo
- Durante lo shutdown, vengono chiuse solo le finestre con marker DALI
- Terminal.app rimane aperto ma senza finestre DALI
- Al prossimo avvio di Terminal, non ci saranno finestre DALI da ripristinare

**Vantaggi**:
- Non interferisce con altre finestre Terminal dell'utente
- Terminal.app rimane utilizzabile dopo lo shutdown di DALI
- Nessun ripristino automatico di finestre DALI

## Note Tecniche

- Le finestre utilizzano dimensioni ridotte del 50% per un miglior utilizzo dello spazio (400x300 su macOS, 40x12 caratteri su Linux)
- Tutte le dimensioni e posizioni sono configurabili tramite costanti all'inizio degli script
- Il posizionamento è relativo allo schermo principale
- Su sistemi multi-monitor, le finestre appariranno sul monitor principale
- La directory temporanea `temp_scripts` viene creata e pulita automaticamente
- Le costanti consentono una facile personalizzazione senza modificare il codice della funzione principale
- **macOS**: Le finestre DALI sono identificate univocamente e chiuse selettivamente

## Changelog

**v2.1** - Risoluzione problema Resume macOS
- Risolto il problema delle finestre che ricompaiono dopo shutdown su macOS
- Implementato sistema di etichettatura delle finestre DALI
- Chiusura selettiva solo delle finestre DALI durante cleanup
- Terminal.app rimane aperto e utilizzabile dopo shutdown DALI

**v2.0** - Aggiunta configurabilità tramite costanti e riduzione dimensioni del 50%
- Definite costanti configurabili all'inizio degli script
- Ridotte le dimensioni delle finestre del 50% per ottimizzare l'uso dello spazio
- Aggiornata documentazione con esempi di personalizzazione

**v1.0** - Implementazione iniziale del posizionamento a cascata 