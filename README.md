# hofl-typing-in-prolog
Higher order functional language typing in prolog.
The program reads a typeable HOFL term and assigns types to it producing a LaTeX representation.

## HOFL

### Terms
![hofl terms](hofl_terms.png)

### Types

τ ::= int | τ0 * τ1 | τ0 → τ1


### Type judgements
![hofl type judgements](hofl_type_judgements.png)

types are assigned to pre-terms using a set of inference rules
(structural induction of HOFL syntax)

### Type system
![hofl type system](hofl_type_system.png)

#### Well Formed term
![well formed term](hofl_well_formed.png)

### Type inference
Type rules are used to derive type constraints (type equations) whose solutions (via unification) define the principal type.

#### Produced typed term
![typing example](hofl_fact_typing.png)