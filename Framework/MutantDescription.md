# Mutant description
Describe what is wrong with the implementations.

## Mutant 1
in file Worker.java function processPublish() changed boundary to <= 10:
userMessage.getMessage().length() > 10

## Mutant 2
replace variables with others from same scope
alice published -> dislike v bob -> disliker und receiver replaced

## Mutant 3
expects that banned after 2 reports:
boundary in isBanned changed maybe to reporters.size() <= 5 :

private boolean isBanned(String clientName) {
HashSet<String> reporters = reports.getOrDefault(clientName, null);
return reporters != null && reporters.size() > 5;
}

## Mutant 4
no mutant

## Mutant 5
no mutant
