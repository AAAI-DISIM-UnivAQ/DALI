#!/bin/bash

# Directory da controllare (puoi modificarla o passare come argomento)
TARGET_DIR=${1:-.}

# Trova tutti i file .sh nella directory e nelle sottodirectory
find "$TARGET_DIR" -type f -name "*.sh" | while read -r file; do
    # Controlla se il file ha terminatori di riga CRLF
    if file "$file" | grep -q "CRLF"; then
        dos2unix "$file"
    else
        echo "$file is already in Unix format."
    fi
done