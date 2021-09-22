MACHINE Results_ProB

EXTENDS Results

CONSTANTS 
    /* CAR */
     Ferrari, Mercedes, Williams, McLaren, Lotus
    

PROPERTIES
    /* CAR */
     Ferrari:CAR & Mercedes:CAR & Williams:CAR &
     McLaren:CAR & Lotus:CAR
        & CAR = {Ferrari, Mercedes, Williams, McLaren, Lotus}
        & card(CAR) = 5
END

