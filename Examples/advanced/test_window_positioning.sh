#!/bin/bash

# Test script per verificare il posizionamento a pila delle finestre
echo "Test di posizionamento a pila delle finestre terminali"

# =================================================================
# CONFIGURAZIONE FINESTRE TERMINALI (come negli script principali)
# =================================================================
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50

# Dimensioni finestre ridotte del 50%
WINDOW_WIDTH_MACOS=400
WINDOW_HEIGHT_MACOS=300
WINDOW_COLS_LINUX=40
WINDOW_ROWS_LINUX=12
# =================================================================

# Configura le variabili come nel file principale
current_dir=$(pwd)
os_name=$(uname -s)
script_counter=0
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y
window_offset=$WINDOW_OFFSET
TEMP_DIR="$current_dir/temp_scripts"

# Array per tracciare le finestre di test create (solo su macOS)
declare -a TEST_WINDOW_IDS=()

# Crea la directory temporanea
mkdir -p "$TEMP_DIR"

# Funzione di test per aprire terminali
test_open_terminal() {
    local cmd="$1"
    local title="$2"
    script_counter=$((script_counter + 1))
    local script_name="$TEMP_DIR/test_script_$script_counter.sh"
    
    # Create test script
    echo "#!/bin/bash" > "$script_name"
    echo "echo \"=== $title ===\"" >> "$script_name"
    echo "echo \"Posizione finestra: $window_x_pos, $window_y_pos\"" >> "$script_name"
    echo "echo \"Comando: $cmd\"" >> "$script_name"
    echo "echo \"Premi Enter per chiudere questa finestra...\"" >> "$script_name"
    echo "read" >> "$script_name"
    chmod 755 "$script_name"
    
    case "$os_name" in
        Darwin)
            echo "Aprendo terminale: $title (posizione $window_x_pos,$window_y_pos, dimensione ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
            if command -v osascript &> /dev/null; then
                window_title="[DALI-TEST] $title"
                osascript -e "
                tell application \"Terminal\"
                    set newTab to do script \"bash '$script_name'\"
                    delay 0.5
                    set custom title of front window to \"$window_title\"
                    set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
                end tell
                " &
                # Salva il titolo della finestra per il cleanup
                TEST_WINDOW_IDS+=("$window_title")
            else
                open -a Terminal "$script_name" &
            fi
            ;;
        Linux)
            echo "Aprendo terminale: $title (posizione $window_x_pos,$window_y_pos, dimensione ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX})"
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash "$script_name" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e "bash $script_name" &
            elif command -v konsole &> /dev/null; then
                konsole --title="$title" --geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e bash "$script_name" &
            else
                echo "Errore: Nessun emulatore di terminale supportato trovato"
                exit 1
            fi
            ;;
    esac
    
    # Incrementa posizione per effetto a cascata
    window_x_pos=$((window_x_pos + window_offset))
    window_y_pos=$((window_y_pos + window_offset))
    
    # Attesa tra le aperture
    sleep 1
}

# Test con 4 finestre simulate
echo "Avvio test con 4 finestre simulate..."
test_open_terminal "echo 'Server DALI simulato'" "Test DALI Server"
test_open_terminal "echo 'Agente 1 simulato'" "Test DALI Agent 1"
test_open_terminal "echo 'Agente 2 simulato'" "Test DALI Agent 2"
test_open_terminal "echo 'Interfaccia utente simulata'" "Test DALI User Interface"

echo ""
echo "Test completato! Dovresti vedere 4 finestre disposte a cascata."
echo ""
echo "Opzioni:"
echo "1. Premi Enter per chiudere automaticamente le finestre di test"
echo "2. Oppure chiudi manualmente le finestre quando hai finito"
read -p "Premi Enter per continuare..."

# Cleanup finestre di test su macOS
if [ "$os_name" = "Darwin" ] && [ ${#TEST_WINDOW_IDS[@]} -gt 0 ]; then
    echo "Chiudendo finestre di test..."
    for window_title in "${TEST_WINDOW_IDS[@]}"; do
        echo "Chiudendo: $window_title"
        osascript -e "
        tell application \"Terminal\"
            repeat with w in windows
                if custom title of w is \"$window_title\" then
                    close w
                end if
            end repeat
        end tell
        " 2>/dev/null || true
    done
    echo "Finestre di test chiuse."
fi

# Cleanup file temporanei
echo "Rimuovendo file temporanei..."
rm -rf "$TEMP_DIR"
echo "Test completato e file temporanei rimossi."
echo "Le finestre di test sono state chiuse automaticamente (su macOS)." 