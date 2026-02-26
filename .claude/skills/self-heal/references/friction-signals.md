# Friction Signal Taxonomy

The self-heal observer watches for the following signals during live skill sessions.

## Signal Categories

### Explicit Repair Signals (High confidence — always log)
- Direct corrections: "No, that's not right", "That's not what I meant", "You misunderstood"
- Reformulations: User restates the same request in different words after receiving a response
- Explicit clarification requests: "What did you mean by X?", "Can you explain why you did Y?"
- Rejection of output: "That's wrong", "Start over", "That's not what I asked for"
- Factual corrections: User provides the correct information after AI provided incorrect information

### Implicit Repair Signals (Medium confidence — log with note)
- "Actually...": Either party uses this to introduce a correction or shift
- "I meant...": User clarifies intent after AI response suggests misalignment
- "Wait...": User pauses to reconsider or redirect
- Repeated clarifying questions: AI asks more than one clarifying question that a clearer skill would not have needed to ask
- Scope creep language: "Well, while we're at it..." suggests the original output didn't fully land
- Hedging by AI: "I think you might mean..." when the skill should have provided enough context

### Self-Initiated Repair Signals (Log as joint friction)
- "Actually, I made a mistake..."
- "Let me correct that..."
- "I misread the requirement..."
- "Looking at this again..."

### Subtle Quality Signals (Low confidence — log only if combined with others)
- "That works, but..." — accepted with reservation
- "Close enough" — settled rather than satisfied
- "I guess that'll do" — resigned acceptance
- User manually fixes something in the output rather than asking AI to fix it

## What Is NOT a Friction Signal

Do not log these as friction:
- Follow-up questions to go deeper (curiosity, not confusion)
- User changing their mind (preference shift, not misrepresentation)
- New information that wasn't available earlier (context update, not failure)
- Normal back-and-forth in collaborative work (dialogue, not friction)

## Logging Format

```
Signal type: [Explicit / Implicit / Self-initiated / Quality]
Signal text: [Quoted excerpt]
Turn: [early / mid / late]
Source hypothesis: [skill-caused or reasoning-caused]
```

This log is held in working memory only — never written to disk during the session.
