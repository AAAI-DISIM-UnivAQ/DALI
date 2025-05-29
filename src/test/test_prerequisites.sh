#!/bin/bash
# src/test/test_prerequisites.sh

echo "Verifica dei prerequisiti per DALI..."
SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus

# Verifica SICStus Prolog
if command -v $SICSTUS &> /dev/null; then
    echo "✓ SICStus Prolog installato"
    $SICSTUS --version
else
    echo "✗ SICStus Prolog non trovato"
    exit 1
fi

# Verifica porte necessarie
echo "Verifica porta 3010..."
if netstat -an | grep -q "3010"; then
    echo "✗ Porta 3010 già in uso"
    exit 1
else
    echo "✓ Porta 3010 disponibile"
fi

echo "Tutti i prerequisiti verificati con successo!"