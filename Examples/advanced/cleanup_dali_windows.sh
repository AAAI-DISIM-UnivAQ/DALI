#!/bin/bash

# =================================================================
# SCRIPT DI UTILITÀ PER CHIUDERE FINESTRE DALI RIMASTE APERTE
# =================================================================
# Questo script cerca e chiude tutte le finestre Terminal con 
# marker DALI che potrebbero essere rimaste aperte
# =================================================================

echo "=== CLEANUP FINESTRE DALI ==="
echo ""

os_name=$(uname -s)

if [ "$os_name" = "Darwin" ]; then
    echo "Cercando finestre DALI aperte su macOS..."
    
    # Cerca tutte le finestre con marker DALI
    dali_windows=$(osascript -e '
    tell application "Terminal"
        set windowList to {}
        repeat with w in windows
            try
                set wTitle to custom title of w
                if wTitle contains "[DALI]" then
                    set end of windowList to wTitle
                end if
            end try
        end repeat
        return windowList
    end tell
    ' 2>/dev/null)
    
    if [ -z "$dali_windows" ] || [ "$dali_windows" = "" ]; then
        echo "Nessuna finestra DALI trovata."
    else
        echo "Finestre DALI trovate:"
        echo "$dali_windows"
        echo ""
        
        read -p "Vuoi chiudere tutte le finestre DALI? (y/n): " choice
        case "$choice" in
            y|Y|yes|YES)
                echo "Chiudendo finestre DALI..."
                osascript -e '
                tell application "Terminal"
                    repeat with w in windows
                        try
                            set wTitle to custom title of w
                            if wTitle contains "[DALI]" then
                                close w
                            end if
                        end try
                    end repeat
                end tell
                ' 2>/dev/null
                echo "Finestre DALI chiuse."
                ;;
            *)
                echo "Operazione annullata."
                ;;
        esac
    fi
    
elif [ "$os_name" = "Linux" ]; then
    echo "Su Linux, le finestre terminali vengono gestite diversamente."
    echo "Per chiudere processi DALI rimasti attivi:"
    echo ""
    echo "1. Controlla processi SICStus Prolog:"
    echo "   ps aux | grep sicstus"
    echo ""
    echo "2. Se necessario, termina i processi:"
    echo "   pkill sicstus"
    echo ""
    echo "3. Controlla terminali aperti:"
    echo "   ps aux | grep -E '(gnome-terminal|xterm|konsole)'"
    echo ""
    
    read -p "Vuoi terminare eventuali processi SICStus rimasti? (y/n): " choice
    case "$choice" in
        y|Y|yes|YES)
            echo "Terminando processi SICStus..."
            pkill sicstus 2>/dev/null || true
            echo "Cleanup completato."
            ;;
        *)
            echo "Operazione annullata."
            ;;
    esac
    
else
    echo "Sistema operativo non supportato: $os_name"
    exit 1
fi

echo ""
echo "=== CLEANUP COMPLETATO ===" 