# Double Events Example

This example demonstrates that it is possible to have conjunctions of external events in the antecedents of a DALI reactive rule.

If you want to define a rule with more than one external event, you must specify the **Simultaneity Interval** before the reactive rule is defined.
The reactive rule will only be executed if all listed external events arrive within that simultaneity interval.

In the advanced example, the agent definitions in the `mas/types` folder have been modified as follows:

---

## Agent Type 1 (`agentType1.txt`)

```prolog
alarm1E :> write('Activate fire suppression and Send security guard '), nl.
```

---

## Agent Type 2 (`agentType2.txt`)

```prolog
alarmE :> write('Call fire department'), nl.
```

---

## Agent Type 3 (`agentType3.txt`)

```prolog
:- dynamic notified/1.
notified(false).

% Simultaneity Interval (T=60)
t60.

% Reactive rule with double events
heat_sensedE(X, T), smoke_detectedE(X, T) :> fire_responceA(X, T).

check_fire(X, T) :- fire_responceP(X, T).
check_fireI(X, T) :> choose_fire(X, T).

choose_fire(X, T) :- 
    notified(false), 
    T < 11, 
    activate_fire_suppressionA, 
    send_security_guardA, 
    retract(notified(false)).

choose_fire(X, T) :- 
    notified(false), 
    T > 10, 
    call_fire_departmentA, 
    retract(notified(false)).

alarm1(X, T) :- activate_fire_suppressionP, send_security_guardP.
alarm1I(X, T) :> messageA(agent1, send_message(alarm1, agent3), agent3), nl.

alarm2(X, T) :- call_fire_departmentP.
alarm2I(X, T) :> messageA(agent2, send_message(alarm, agent3), agent3), nl.

send_security_guard(X, T) :< security_guard_available(X, T).
```
