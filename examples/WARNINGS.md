# Warnings And Timers

This note explains the warning shown by DALI when an agent does not declare a default time window for grouping close external events.

## The `t60.` Declaration

If you add this fact at the beginning of an agent:

```prolog
t60.
```

the agent uses a default time window of 60 seconds. Within that window, two consecutive external events can be treated as simultaneous.

## What Happens If It Is Missing

If the declaration is not present, each agent instance may print a warning at startup to signal that no default simultaneity window has been defined.

## Why It Matters

This setting is useful when your agent logic depends on conjunctions of external events or on event grouping behavior.

For a related example, see [DOUBLE_EVENTS.md](DOUBLE_EVENTS.md).
