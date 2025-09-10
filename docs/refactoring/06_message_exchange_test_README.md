# Test 06: Message Exchange Test

## Descrizione

Questo test verifica il corretto scambio di messaggi tra agenti DALI utilizzando un approccio temporizzato strutturato. Integra la logica completa di avvio del sistema DALI con fasi di test specifiche per monitorare la comunicazione inter-agente.

## Funzionalità

### Fasi di Test

1. **Building Agent Instances** - Costruzione degli agenti di test
2. **Starting DALI Components** - Avvio server, agenti e interfaccia utente  
3. **System Initialization** - Attesa per stabilizzazione sistema (8s)
4. **Message Exchange Testing** - Cicli di test per scambio messaggi
5. **Final Monitoring Period** - Periodo di osservazione finale
6. **System Information** - Riepilogo risultati test
7. **Shutdown** - Pulizia ordinata del sistema

### Parametri Configurabili

```bash
STARTUP_WAIT_TIME=8          # Tempo attesa dopo avvio componenti
MESSAGE_TEST_INTERVAL=10     # Durata ogni ciclo di test messaggi  
TEST_CYCLES=3                # Numero cicli di test
STABILIZATION_TIME=5         # Pausa tra cicli di test
SHUTDOWN_DELAY=3             # Attesa prima dello shutdown
```

### Agenti di Test

Il test utilizza gli agenti originali più un agente di test per lo scambio messaggi:

**Agent1** (da agentType1.txt): 

- Stampa "Hello world!" all'avvio
- Gestisce evento `eventE` e `goE`
- Su `goE`: stampa "received." e invia messaggio a agent2
- Mantiene la logica originale del tipo

**Agent2** (da agentType2.txt):

- Gestisce evento `goE` stampando "received"
- Riceve messaggi da altri agenti
- Mantiene la logica originale del tipo

**Test Sender** (Agente di Test - `test_agent.txt`):

- **File sorgente**: `src/test/scripts/test_agent.txt`
- **Solo invio messaggi**: Non risponde ad eventi, solo invia
- **Timer DALI**: `t10` per esecuzione periodica
- **Regole interne**: `start_testingI` e `send_test_messagesI`
- **Target**: Invia messaggi sia ad agent1 che ad agent2
- **Sequenza**: Invia `go` (triggera goE), poi `test_msg` e `hello`

## Utilizzo

```bash
cd /Users/giodegas/ai/DALI_2024/DALI/src/test/scripts
./06_message_exchange_test.sh
```

## Output Atteso

Il test dovrebbe mostrare:

1. **Avvio Ordinato**: Server → Agent1, Agent2, Test Sender → Interfaccia Utente
2. **Scambio Messaggi Automatico**: 
   - Test Sender inizia dopo 5 secondi inviando `go` ad agent1 e agent2
   - Agent1 riceve `goE`, stampa "received." e invia messaggio ad agent2
   - Agent2 riceve `goE` e stampa "received"
   - Tutti i messaggi sono visibili nelle finestre degli agenti
3. **Osservazione Strutturata**: 
   - 3 cicli di osservazione da 10 secondi ciascuno
   - Logging automatico di tutti gli scambi di messaggi
   - Pause di stabilizzazione tra i cicli
4. **Shutdown Pulito**: Chiusura ordinata di tutti i componenti

## Requisiti

- SICStus Prolog installato
- Porta 3010 disponibile
- Sistema operativo: macOS o Linux
- Directory DALI core accessibile (`../`)

## Compatibilità

- **macOS**: Utilizza Terminal.app con AppleScript per gestione finestre
- **Linux**: Utilizza gnome-terminal per gestione finestre

## Note

- Il test include auto-shutdown dopo 10 secondi se non viene premuto Enter
- I percorsi sono ottimizzati per l'esecuzione dalla directory `src/test/scripts`
- Gli agenti di test sono creati dinamicamente se non esistono file di configurazione predefiniti
- Tutte le finestre DALI vengono chiuse automaticamente al termine
