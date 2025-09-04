#!/bin/bash

echo "=== DALI Debug Test ==="
echo "Verificando le correzioni apportate al sistema..."
echo ""

echo "1. Controllo sintassi test_agent.txt:"
echo "   Messaggio corretto: messageA(agent1, send_message(go, test_sender))"
grep -n "messageA" src/test/scripts/test_agent.txt

echo ""
echo "2. Controllo definizione messageA in dali_core.pl:"
grep -n -A 3 "messageA" src/dali_core.pl

echo ""
echo "3. Controllo export di messageA nel modulo:"
grep -n -A 8 "module(dali_core" src/dali_core.pl

echo ""
echo "4. Controllo agenti target:"
echo "   agent1.txt ha regola goE:>"
grep -n "goE" Examples/advanced/mas/types/agentType1.txt
echo "   agent2.txt ha regola goE:>"
grep -n "goE" Examples/advanced/mas/types/agentType2.txt

echo ""
echo "=== Debug completato ==="
echo "Le correzioni dovrebbero permettere il corretto funzionamento del test."
echo "Ora puoi eseguire lo script 06_message_exchange_test.sh"
