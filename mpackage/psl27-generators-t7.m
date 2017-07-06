pslA3 = {{(I*(et^2 - et^5))/Sqrt[7], (I*(et - et^6))/Sqrt[7], 
      (I*(-et^3 + et^4))/Sqrt[7]}, {(I*(et - et^6))/Sqrt[7], 
      (I*(-et^3 + et^4))/Sqrt[7], (I*(et^2 - et^5))/Sqrt[7]}, 
     {(I*(-et^3 + et^4))/Sqrt[7], (I*(et^2 - et^5))/Sqrt[7], 
      (I*(et - et^6))/Sqrt[7]}}
 
pslB3 = {{(I*(et^3 - et^6))/Sqrt[7], (I*(-et + et^3))/Sqrt[7], 
      (I*(-1 + et))/Sqrt[7]}, {(I*(-1 + et^2))/Sqrt[7], 
      (I*(-et^5 + et^6))/Sqrt[7], (I*(-et^2 + et^6))/Sqrt[7]}, 
     {(I*(-et^4 + et^5))/Sqrt[7], (I*(-1 + et^4))/Sqrt[7], 
      (I*(-et^3 + et^5))/Sqrt[7]}}
 
pslA3b = {{I*(et^2/Sqrt[7] - et^5/Sqrt[7]), I*(et/Sqrt[7] - et^6/Sqrt[7]), 
      I*(-(et^3/Sqrt[7]) + et^4/Sqrt[7])}, {I*(et/Sqrt[7] - et^6/Sqrt[7]), 
      I*(-(et^3/Sqrt[7]) + et^4/Sqrt[7]), I*(et^2/Sqrt[7] - et^5/Sqrt[7])}, 
     {I*(-(et^3/Sqrt[7]) + et^4/Sqrt[7]), I*(et^2/Sqrt[7] - et^5/Sqrt[7]), 
      I*(et/Sqrt[7] - et^6/Sqrt[7])}}
 
pslB3b = {{I*(et/Sqrt[7] - et^4/Sqrt[7]), I*(-(et^4/Sqrt[7]) + et^6/Sqrt[7]), 
      I*(1/Sqrt[7] - et^6/Sqrt[7])}, {I*(1/Sqrt[7] - et^5/Sqrt[7]), 
      I*(-(et/Sqrt[7]) + et^2/Sqrt[7]), I*(-(et/Sqrt[7]) + et^5/Sqrt[7])}, 
     {I*(-(et^2/Sqrt[7]) + et^3/Sqrt[7]), I*(1/Sqrt[7] - et^3/Sqrt[7]), 
      I*(-(et^2/Sqrt[7]) + et^4/Sqrt[7])}}
 
pslA6 = {{2/7 - et^2/7 - et^5/7, 3/7 + et^2/7 + et^3/7 + et^4/7 + et^5/7, 
      2/7 - et^3/7 - et^4/7, -Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - 
       (2*Sqrt[2]*et^3)/7 - (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      -(Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 - 
       (Sqrt[2]*et^5)/7, Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7 + (2*Sqrt[2]*et^5)/7}, 
     {3/7 + et^2/7 + et^3/7 + et^4/7 + et^5/7, 2/7 - et^3/7 - et^4/7, 
      2/7 - et^2/7 - et^5/7, -(Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + 
       (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 + (2*Sqrt[2]*et^5)/7, 
      -Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - (2*Sqrt[2]*et^3)/7 - 
       (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7}, {2/7 - et^3/7 - et^4/7, 
      2/7 - et^2/7 - et^5/7, 3/7 + et^2/7 + et^3/7 + et^4/7 + et^5/7, 
      Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 + 
       (2*Sqrt[2]*et^5)/7, -Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - 
       (2*Sqrt[2]*et^3)/7 - (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      -(Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 - 
       (Sqrt[2]*et^5)/7}, {-Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - 
       (2*Sqrt[2]*et^3)/7 - (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      -(Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 - 
       (Sqrt[2]*et^5)/7, Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7 + (2*Sqrt[2]*et^5)/7, 2/7 - et^2/7 - et^5/7, 
      3/7 + et^2/7 + et^3/7 + et^4/7 + et^5/7, 2/7 - et^3/7 - et^4/7}, 
     {-(Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 - 
       (Sqrt[2]*et^5)/7, Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7 + (2*Sqrt[2]*et^5)/7, -Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - 
       (2*Sqrt[2]*et^3)/7 - (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      3/7 + et^2/7 + et^3/7 + et^4/7 + et^5/7, 2/7 - et^3/7 - et^4/7, 
      2/7 - et^2/7 - et^5/7}, {Sqrt[2]/7 + (2*Sqrt[2]*et^2)/7 + 
       (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 + (2*Sqrt[2]*et^5)/7, 
      -Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - (2*Sqrt[2]*et^3)/7 - 
       (2*Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, -(Sqrt[2]*et^2)/7 + 
       (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      2/7 - et^3/7 - et^4/7, 2/7 - et^2/7 - et^5/7, 3/7 + et^2/7 + et^3/7 + 
       et^4/7 + et^5/7}}
 
pslB6 = {{1/7 + (3*et)/7 + et^2/7 + et^4/7 + et^5/7, 
      -et/7 + (2*et^2)/7 - et^3/7, -1/7 - et/7 + (2*et^4)/7, 
      Sqrt[2]/7 - (Sqrt[2]*et^2)/7 - (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^5)/7, 
      -Sqrt[2]/7 + (Sqrt[2]*et)/7 + (Sqrt[2]*et^2)/7 - (Sqrt[2]*et^3)/7, 
      (Sqrt[2]*et)/7 - (Sqrt[2]*et^2)/7 - (Sqrt[2]*et^4)/7 + 
       (Sqrt[2]*et^5)/7}, {-1/7 + (2*et)/7 - et^2/7, 
      1/7 + et/7 + (3*et^2)/7 + et^3/7 + et^4/7, 1/7 + et/7 + et^3/7 + 
       (3*et^4)/7 + et^5/7, -(Sqrt[2]*et)/7 + (Sqrt[2]*et^2)/7 + 
       (Sqrt[2]*et^3)/7 - (Sqrt[2]*et^4)/7, (2*Sqrt[2])/7 + (Sqrt[2]*et)/7 + 
       (Sqrt[2]*et^2)/7 + (2*Sqrt[2]*et^3)/7 + (Sqrt[2]*et^5)/7, 
      (Sqrt[2]*et)/7 + (2*Sqrt[2]*et^2)/7 + (Sqrt[2]*et^3)/7 + 
       (2*Sqrt[2]*et^4)/7 + (Sqrt[2]*et^5)/7}, {(2*et)/7 - et^4/7 - et^5/7, 
      -1/7 + (2*et^2)/7 - et^4/7, -et^3/7 + (2*et^4)/7 - et^5/7, 
      -Sqrt[2]/7 + (Sqrt[2]*et)/7 + (Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      -Sqrt[2]/7 - (2*Sqrt[2]*et)/7 - (2*Sqrt[2]*et^2)/7 - (Sqrt[2]*et^3)/7 - 
       (Sqrt[2]*et^5)/7, (-2*Sqrt[2]*et)/7 - (Sqrt[2]*et^2)/7 - 
       (Sqrt[2]*et^3)/7 - (Sqrt[2]*et^4)/7 - (2*Sqrt[2]*et^5)/7}, 
     {Sqrt[2]/7 + (Sqrt[2]*et^2)/7 - (Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      (-2*Sqrt[2])/7 - (Sqrt[2]*et)/7 - (Sqrt[2]*et^2)/7 - (Sqrt[2]*et^3)/7 - 
       (2*Sqrt[2]*et^4)/7, -Sqrt[2]/7 - (Sqrt[2]*et)/7 - (2*Sqrt[2]*et^3)/7 - 
       (Sqrt[2]*et^4)/7 - (2*Sqrt[2]*et^5)/7, -2/7 - (3*et)/7 - (2*et^2)/7 - 
       (2*et^3)/7 - (3*et^4)/7 - (2*et^5)/7, 1/7 + et/7 + et^2/7 + et^3/7 + 
       (3*et^5)/7, et/7 + et^2/7 + (3*et^3)/7 + et^4/7 + et^5/7}, 
     {Sqrt[2]/7 + (Sqrt[2]*et)/7 + (Sqrt[2]*et^2)/7 + (2*Sqrt[2]*et^4)/7 + 
       (2*Sqrt[2]*et^5)/7, Sqrt[2]/7 - (Sqrt[2]*et)/7 - (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7, -Sqrt[2]/7 - (Sqrt[2]*et)/7 + (Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^5)/7, -3/7 - (2*et)/7 - (2*et^2)/7 - (2*et^3)/7 - 
       (2*et^4)/7 - (3*et^5)/7, -et/7 - et^2/7 + (2*et^5)/7, 
      -et/7 + (2*et^3)/7 - et^5/7}, {(-2*Sqrt[2])/7 - (Sqrt[2]*et)/7 - 
       (2*Sqrt[2]*et^2)/7 - (Sqrt[2]*et^4)/7 - (Sqrt[2]*et^5)/7, 
      Sqrt[2]/7 + (2*Sqrt[2]*et)/7 + (Sqrt[2]*et^2)/7 + (2*Sqrt[2]*et^3)/7 + 
       (Sqrt[2]*et^4)/7, (2*Sqrt[2])/7 + (2*Sqrt[2]*et)/7 + 
       (Sqrt[2]*et^3)/7 + (Sqrt[2]*et^4)/7 + (Sqrt[2]*et^5)/7, 
      -2/7 - (2*et)/7 - (3*et^2)/7 - (3*et^3)/7 - (2*et^4)/7 - (2*et^5)/7, 
      -1/7 - et^3/7 + (2*et^5)/7, -et^2/7 + (2*et^3)/7 - et^4/7}}
 
pslA7 = {{((I/7)*(1 + 2*et + 2*et^2 + 2*et^4))/Sqrt[7], 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4)}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7], 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7], 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7], 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7], 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7], 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7], 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7], 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7], 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7], 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7], 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7], 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7], 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7], 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7], 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7], 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7], 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7], 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7], 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7], 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7], 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7], 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7], 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7], 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7], 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7], 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7]*(1 + 2*et + 2*et^2 + 2*et^4), 
      (((-2*I)/7)*(1 + 2*et - et^3 + 3*et^4 + 2*et^5))/Sqrt[7], 
      (((2*I)/7)*(1 + 2*et - et^2 + 2*et^3 + 3*et^5))/Sqrt[7], 
      (((-2*I)/7)*(2 + 4*et + 3*et^2 + 3*et^3 + et^4 + et^5))/Sqrt[7], 
      ((I/7)*(4 + 8*et + 4*et^2 + 5*et^3 + 3*et^4 + 4*et^5))/Sqrt[7], 
      ((I/7)*et^2*(-1 - 4*et + 4*et^2 + et^3))/Sqrt[7], 
      ((-I/7)*(1 + 2*et - 3*et^2 + et^3 + et^4 + 5*et^5))/Sqrt[7]}}
 
pslB7 = {{(I/7)/Sqrt[7] + (((2*I)/7)*et)/Sqrt[7] + (((2*I)/7)*et^2)/Sqrt[7] + 
       (((2*I)/7)*et^4)/Sqrt[7], ((2*I)/7)*Sqrt[2/7]*et + 
       ((4*I)/7)*Sqrt[2/7]*et^2 + ((4*I)/7)*Sqrt[2/7]*et^3 + 
       ((4*I)/7)*Sqrt[2/7]*et^5, ((-4*I)/7)*Sqrt[2/7] - 
       ((4*I)/7)*Sqrt[2/7]*et - ((2*I)/7)*Sqrt[2/7]*et^2 - 
       ((4*I)/7)*Sqrt[2/7]*et^5, ((-4*I)/7)*Sqrt[2/7] - 
       ((4*I)/7)*Sqrt[2/7]*et^2 - ((4*I)/7)*Sqrt[2/7]*et^3 - 
       ((2*I)/7)*Sqrt[2/7]*et^4, ((2*I)/7)*Sqrt[2/7] + 
       ((2*I)/7)*Sqrt[2/7]*et - ((2*I)/7)*Sqrt[2/7]*et^2 + 
       ((2*I)/7)*Sqrt[2/7]*et^3 - ((2*I)/7)*Sqrt[2/7]*et^4 - 
       ((2*I)/7)*Sqrt[2/7]*et^5, ((-4*I)/7)*Sqrt[2/7]*et - 
       ((4*I)/7)*Sqrt[2/7]*et^3 - ((4*I)/7)*Sqrt[2/7]*et^4 - 
       ((2*I)/7)*Sqrt[2/7]*et^5, ((4*I)/7)*Sqrt[2/7] + 
       ((2*I)/7)*Sqrt[2/7]*et^3 + ((4*I)/7)*Sqrt[2/7]*et^4 + 
       ((4*I)/7)*Sqrt[2/7]*et^5}, {((2*I)/7)*Sqrt[2/7] + 
       ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*et^2 + 
       ((4*I)/7)*Sqrt[2/7]*et^4, (-I/7)/Sqrt[7] - ((I/7)*et)/Sqrt[7] - 
       ((I/7)*et^2)/Sqrt[7] - (((2*I)/7)*et^3)/Sqrt[7] - 
       (((5*I)/7)*et^4)/Sqrt[7] + (((3*I)/7)*et^5)/Sqrt[7], 
      ((-4*I)/7)/Sqrt[7] + ((I/7)*et)/Sqrt[7] - ((I/7)*et^3)/Sqrt[7] + 
       (((4*I)/7)*et^4)/Sqrt[7], (I/7)/Sqrt[7] - ((I/7)*et)/Sqrt[7] - 
       (((4*I)/7)*et^3)/Sqrt[7] + (((4*I)/7)*et^5)/Sqrt[7], 
      ((2*I)/7)/Sqrt[7] - (((4*I)/7)*et)/Sqrt[7] + (((2*I)/7)*et^2)/Sqrt[7] - 
       (((2*I)/7)*et^3)/Sqrt[7] + (((4*I)/7)*et^4)/Sqrt[7] - 
       (((2*I)/7)*et^5)/Sqrt[7], ((2*I)/7)/Sqrt[7] + (((2*I)/7)*et)/Sqrt[7] + 
       (((6*I)/7)*et^2)/Sqrt[7] + (((6*I)/7)*et^3)/Sqrt[7] + 
       (((8*I)/7)*et^4)/Sqrt[7] + (((4*I)/7)*et^5)/Sqrt[7], 
      ((-8*I)/7)/Sqrt[7] - (((6*I)/7)*et)/Sqrt[7] - (((2*I)/7)*et^2)/
        Sqrt[7] - (((4*I)/7)*et^3)/Sqrt[7] - (((6*I)/7)*et^4)/Sqrt[7] - 
       (((2*I)/7)*et^5)/Sqrt[7]}, {((2*I)/7)*Sqrt[2/7] + 
       ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*et^2 + 
       ((4*I)/7)*Sqrt[2/7]*et^4, ((5*I)/7)/Sqrt[7] + (((4*I)/7)*et)/Sqrt[7] + 
       (((3*I)/7)*et^2)/Sqrt[7] + (((8*I)/7)*et^3)/Sqrt[7] + 
       (((4*I)/7)*et^4)/Sqrt[7] + (((4*I)/7)*et^5)/Sqrt[7], 
      (I/7)/Sqrt[7] - (((3*I)/7)*et)/Sqrt[7] + ((I/7)*et^2)/Sqrt[7] + 
       (((5*I)/7)*et^3)/Sqrt[7] + ((I/7)*et^4)/Sqrt[7] + 
       (((2*I)/7)*et^5)/Sqrt[7], ((-3*I)/7)/Sqrt[7] + 
       (((5*I)/7)*et)/Sqrt[7] + (((2*I)/7)*et^2)/Sqrt[7] + 
       ((I/7)*et^3)/Sqrt[7] + ((I/7)*et^4)/Sqrt[7] + ((I/7)*et^5)/Sqrt[7], 
      ((-4*I)/7)/Sqrt[7] - (((2*I)/7)*et)/Sqrt[7] - (((2*I)/7)*et^2)/
        Sqrt[7] + (((2*I)/7)*et^3)/Sqrt[7] + (((2*I)/7)*et^4)/Sqrt[7] + 
       (((4*I)/7)*et^5)/Sqrt[7], ((4*I)/7)/Sqrt[7] + (((6*I)/7)*et)/Sqrt[7] - 
       (((2*I)/7)*et^2)/Sqrt[7] + (((4*I)/7)*et^4)/Sqrt[7] + 
       (((2*I)/7)*et^5)/Sqrt[7], ((-4*I)/7)/Sqrt[7] + 
       (((2*I)/7)*et)/Sqrt[7] - (((4*I)/7)*et^2)/Sqrt[7] - 
       (((2*I)/7)*et^3)/Sqrt[7] - (((6*I)/7)*et^5)/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7] + ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*
        et^2 + ((4*I)/7)*Sqrt[2/7]*et^4, ((-4*I)/7)/Sqrt[7] + 
       (((4*I)/7)*et^2)/Sqrt[7] + ((I/7)*et^4)/Sqrt[7] - 
       ((I/7)*et^5)/Sqrt[7], ((-3*I)/7)/Sqrt[7] - (((4*I)/7)*et)/Sqrt[7] - 
       (((4*I)/7)*et^2)/Sqrt[7] - (((4*I)/7)*et^3)/Sqrt[7] - 
       (((5*I)/7)*et^4)/Sqrt[7] - (((8*I)/7)*et^5)/Sqrt[7], 
      ((-4*I)/7)/Sqrt[7] - (((4*I)/7)*et)/Sqrt[7] - (((8*I)/7)*et^2)/
        Sqrt[7] - (((3*I)/7)*et^3)/Sqrt[7] - (((4*I)/7)*et^4)/Sqrt[7] - 
       (((5*I)/7)*et^5)/Sqrt[7], ((-2*I)/7)/Sqrt[7] + 
       (((2*I)/7)*et)/Sqrt[7] + (((4*I)/7)*et^2)/Sqrt[7] - 
       (((4*I)/7)*et^3)/Sqrt[7] - (((2*I)/7)*et^4)/Sqrt[7] + 
       (((2*I)/7)*et^5)/Sqrt[7], ((-6*I)/7)/Sqrt[7] - 
       (((4*I)/7)*et^2)/Sqrt[7] + (((2*I)/7)*et^3)/Sqrt[7] - 
       (((4*I)/7)*et^4)/Sqrt[7] - (((2*I)/7)*et^5)/Sqrt[7], 
      ((4*I)/7)/Sqrt[7] + (((4*I)/7)*et)/Sqrt[7] + (((6*I)/7)*et^2)/Sqrt[7] + 
       (((2*I)/7)*et^3)/Sqrt[7] - (((2*I)/7)*et^4)/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7] + ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*
        et^2 + ((4*I)/7)*Sqrt[2/7]*et^4, ((-6*I)/7)/Sqrt[7] - 
       (((4*I)/7)*et)/Sqrt[7] - (((2*I)/7)*et^2)/Sqrt[7] - 
       (((8*I)/7)*et^3)/Sqrt[7] - (((2*I)/7)*et^4)/Sqrt[7] - 
       (((6*I)/7)*et^5)/Sqrt[7], (((2*I)/7)*et)/Sqrt[7] - 
       (((2*I)/7)*et^2)/Sqrt[7] - (((6*I)/7)*et^3)/Sqrt[7] - 
       (((4*I)/7)*et^4)/Sqrt[7] - (((4*I)/7)*et^5)/Sqrt[7], 
      ((2*I)/7)/Sqrt[7] - (((6*I)/7)*et)/Sqrt[7] - (((4*I)/7)*et^2)/Sqrt[7] - 
       (((2*I)/7)*et^4)/Sqrt[7] - (((4*I)/7)*et^5)/Sqrt[7], 
      ((-I/7)*et)/Sqrt[7] - (((4*I)/7)*et^2)/Sqrt[7] + 
       (((4*I)/7)*et^3)/Sqrt[7] + ((I/7)*et^4)/Sqrt[7], 
      ((5*I)/7)/Sqrt[7] + ((I/7)*et)/Sqrt[7] + ((I/7)*et^2)/Sqrt[7] - 
       (((3*I)/7)*et^3)/Sqrt[7] + (((2*I)/7)*et^4)/Sqrt[7] + 
       ((I/7)*et^5)/Sqrt[7], ((-2*I)/7)/Sqrt[7] - ((I/7)*et)/Sqrt[7] - 
       (((5*I)/7)*et^2)/Sqrt[7] - ((I/7)*et^3)/Sqrt[7] + 
       (((3*I)/7)*et^4)/Sqrt[7] - ((I/7)*et^5)/Sqrt[7]}, 
     {((2*I)/7)*Sqrt[2/7] + ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*
        et^2 + ((4*I)/7)*Sqrt[2/7]*et^4, ((2*I)/7)/Sqrt[7] - 
       (((2*I)/7)*et)/Sqrt[7] - (((6*I)/7)*et^2)/Sqrt[7] - 
       (((4*I)/7)*et^3)/Sqrt[7] - (((4*I)/7)*et^4)/Sqrt[7], 
      ((2*I)/7)/Sqrt[7] + (((6*I)/7)*et)/Sqrt[7] + (((4*I)/7)*et^2)/Sqrt[7] + 
       (((2*I)/7)*et^3)/Sqrt[7] + (((6*I)/7)*et^4)/Sqrt[7] + 
       (((8*I)/7)*et^5)/Sqrt[7], ((6*I)/7)/Sqrt[7] + (((2*I)/7)*et)/Sqrt[7] + 
       (((8*I)/7)*et^2)/Sqrt[7] + (((2*I)/7)*et^3)/Sqrt[7] + 
       (((4*I)/7)*et^4)/Sqrt[7] + (((6*I)/7)*et^5)/Sqrt[7], 
      (-I/7)/Sqrt[7] + (((4*I)/7)*et)/Sqrt[7] - (((4*I)/7)*et^4)/Sqrt[7] + 
       ((I/7)*et^5)/Sqrt[7], ((-4*I)/7)/Sqrt[7] - (((3*I)/7)*et)/Sqrt[7] - 
       (((5*I)/7)*et^2)/Sqrt[7] - (((4*I)/7)*et^3)/Sqrt[7] - 
       (((8*I)/7)*et^4)/Sqrt[7] - (((4*I)/7)*et^5)/Sqrt[7], 
      ((8*I)/7)/Sqrt[7] + (((5*I)/7)*et)/Sqrt[7] + (((4*I)/7)*et^2)/Sqrt[7] + 
       (((4*I)/7)*et^3)/Sqrt[7] + (((4*I)/7)*et^4)/Sqrt[7] + 
       (((3*I)/7)*et^5)/Sqrt[7]}, {((2*I)/7)*Sqrt[2/7] + 
       ((4*I)/7)*Sqrt[2/7]*et + ((4*I)/7)*Sqrt[2/7]*et^2 + 
       ((4*I)/7)*Sqrt[2/7]*et^4, ((4*I)/7)/Sqrt[7] + (((2*I)/7)*et)/Sqrt[7] + 
       (((4*I)/7)*et^3)/Sqrt[7] + (((6*I)/7)*et^4)/Sqrt[7] - 
       (((2*I)/7)*et^5)/Sqrt[7], ((6*I)/7)/Sqrt[7] + 
       (((2*I)/7)*et^2)/Sqrt[7] + (((4*I)/7)*et^3)/Sqrt[7] - 
       (((2*I)/7)*et^4)/Sqrt[7] + (((4*I)/7)*et^5)/Sqrt[7], 
      (((4*I)/7)*et)/Sqrt[7] + (((4*I)/7)*et^2)/Sqrt[7] + 
       (((6*I)/7)*et^3)/Sqrt[7] + (((2*I)/7)*et^4)/Sqrt[7] - 
       (((2*I)/7)*et^5)/Sqrt[7], ((4*I)/7)/Sqrt[7] + ((I/7)*et^2)/Sqrt[7] - 
       ((I/7)*et^3)/Sqrt[7] - (((4*I)/7)*et^5)/Sqrt[7], 
      (-I/7)/Sqrt[7] - (((4*I)/7)*et)/Sqrt[7] + (((4*I)/7)*et^2)/Sqrt[7] + 
       ((I/7)*et^3)/Sqrt[7], (((-4*I)/7)*et)/Sqrt[7] + ((I/7)*et^2)/Sqrt[7] - 
       ((I/7)*et^4)/Sqrt[7] + (((4*I)/7)*et^5)/Sqrt[7]}}
 
pslA8 = {{0, (I/14)*(I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5), 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14}, 
     {(-I/14)*(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5), 0, 
      (-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (2*I - (-3*I + Sqrt[3])*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (2*I - (-3*I + Sqrt[3])*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14}, 
     {(-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7, 
      (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7, -((-1 + et)^2*et^2*(1 + et))/7, 
      (2 - et^2 - et^5)/7, (3 + et^2 + et^3 + et^4 + et^5)/7, 
      (2 - et^3 - et^4)/7}, {(2*I - (-3*I + Sqrt[3])*et^2 + 
        (3*I + Sqrt[3])*et^3 + (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/
       14, (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7, -((-1 + et)^2*et^2*(1 + et))/7, 
      (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7, (3 + et^2 + et^3 + et^4 + et^5)/
       7, (2 - et^3 - et^4)/7, (2 - et^2 - et^5)/7}, 
     {(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      -((-1 + et)^2*et^2*(1 + et))/7, (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7, 
      (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7, (2 - et^3 - et^4)/7, 
      (2 - et^2 - et^5)/7, (3 + et^2 + et^3 + et^4 + et^5)/7}, 
     {(-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, (2 - et^2 - et^5)/7, 
      (3 + et^2 + et^3 + et^4 + et^5)/7, (2 - et^3 - et^4)/7, 
      (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7, 
      (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7, -((-1 + et)^2*et^2*(1 + et))/
       7}, {(2*I - (-3*I + Sqrt[3])*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (3 + et^2 + et^3 + et^4 + et^5)/7, (2 - et^3 - et^4)/7, 
      (2 - et^2 - et^5)/7, (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7, 
      -((-1 + et)^2*et^2*(1 + et))/7, (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7}, 
     {(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, (2 - et^3 - et^4)/7, 
      (2 - et^2 - et^5)/7, (3 + et^2 + et^3 + et^4 + et^5)/7, 
      -((-1 + et)^2*et^2*(1 + et))/7, (1 + 2*et^2 + et^3 + et^4 + 2*et^5)/7, 
      (-1 - et^2 - 2*et^3 - 2*et^4 - et^5)/7}}
 
pslB8 = {{0, (I/14)*(I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5), 
      (-3*I + Sqrt[3] - (2*I)*et + (-3*I + Sqrt[3])*et^2 - 
        (3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (-2*Sqrt[3] - (-3*I + Sqrt[3])*et - (-I + Sqrt[3])*et^2 - 
        (-3*I + Sqrt[3])*et^3 - 2*Sqrt[3]*et^4)/14, 
      (3*I - Sqrt[3] - (-3*I + Sqrt[3])*et - 2*Sqrt[3]*et^3 - 
        (-I + Sqrt[3])*et^4 - 2*Sqrt[3]*et^5)/14, 
      ((-1 + et)^2*(1 + et)*(-I + Sqrt[3] + (I + Sqrt[3])*et + 
         (-I + Sqrt[3])*et^2))/14, (-3*I - Sqrt[3] + (-3*I + Sqrt[3])*et + 
        (-3*I + Sqrt[3])*et^2 - (3*I + Sqrt[3])*et^3 - (2*I)*et^5)/14, 
      (et*(-3*I + Sqrt[3] - (3*I + Sqrt[3])*et - (2*I)*et^2 - 
         (3*I + Sqrt[3])*et^3 + (-3*I + Sqrt[3])*et^4))/14}, 
     {(-I/14)*(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5), 0, 
      (3*I + Sqrt[3] + (2*I)*et + (3*I + Sqrt[3])*et^2 - 
        (-3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*Sqrt[3] - (3*I + Sqrt[3])*et - (I + Sqrt[3])*et^2 - 
        (3*I + Sqrt[3])*et^3 - 2*Sqrt[3]*et^4)/14, 
      (-3*I - Sqrt[3] - (3*I + Sqrt[3])*et - 2*Sqrt[3]*et^3 - 
        (I + Sqrt[3])*et^4 - 2*Sqrt[3]*et^5)/14, 
      ((-1 + et)^2*(1 + et)*(I + Sqrt[3] + (-I + Sqrt[3])*et + 
         (I + Sqrt[3])*et^2))/14, (3*I - Sqrt[3] + (3*I + Sqrt[3])*et + 
        (3*I + Sqrt[3])*et^2 - (-3*I + Sqrt[3])*et^3 + (2*I)*et^5)/14, 
      (et*(3*I + Sqrt[3] - (-3*I + Sqrt[3])*et + (2*I)*et^2 - 
         (-3*I + Sqrt[3])*et^3 + (3*I + Sqrt[3])*et^4))/14}, 
     {(-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2 - et - 2*et^2 - et^4 - et^5)/7, (1 + 2*et + et^2 + 2*et^3 + et^4)/
       7, (2 + 2*et + et^3 + et^4 + et^5)/7, 
      (-2 - 3*et - 2*et^2 - 2*et^3 - 3*et^4 - 2*et^5)/7, 
      (1 + et + et^2 + et^3 + 3*et^5)/7, (et*(1 + et + 3*et^2 + et^3 + et^4))/
       7}, {(2*I - (-3*I + Sqrt[3])*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (1 + et^2 - et^4 - et^5)/7, (-2 - et - et^2 - et^3 - 2*et^4)/7, 
      (-1 - et - 2*et^3 - et^4 - 2*et^5)/7, 
      (-3 - 2*et - 2*et^2 - 2*et^3 - 2*et^4 - 3*et^5)/7, 
      (et*(-1 - et + 2*et^4))/7, -(et*(-1 + et^2)^2)/7}, 
     {(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (1 + et + et^2 + 2*et^4 + 2*et^5)/7, ((-1 + et)^2*(1 + et + et^2))/7, 
      (-1 - et + et^3 + et^5)/7, (-2 - 2*et - 3*et^2 - 3*et^3 - 2*et^4 - 
        2*et^5)/7, (-1 - et^3 + 2*et^5)/7, -((-1 + et)^2*et^2)/7}, 
     {(-I - Sqrt[3] - (3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (3*I + Sqrt[3])*et^5)/14, 
      (I - Sqrt[3] - (-3*I + Sqrt[3])*et^2 - 2*Sqrt[3]*et^3 - 
        2*Sqrt[3]*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (1 + 3*et + et^2 + et^4 + et^5)/7, -((-1 + et)^2*et)/7, 
      (-1 - et + 2*et^4)/7, (-1 + et + et^4 - et^5)/7, 
      (-1 - 2*et - 2*et^2 - et^3 - et^5)/7, 
      -(et*(2 + et + et^2 + et^3 + 2*et^4))/7}, 
     {(2*I - (-3*I + Sqrt[3])*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 - (-3*I + Sqrt[3])*et^5)/14, 
      (-2*I - (3*I + Sqrt[3])*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 - (3*I + Sqrt[3])*et^5)/14, -(-1 + et)^2/7, 
      (1 + et + 3*et^2 + et^3 + et^4)/7, (1 + et + et^3 + 3*et^4 + et^5)/7, 
      (1 - et^2 - et^3 + et^5)/7, -((-1 + et)^2*(1 + et))/7, 
      ((-1 + et)^2*et*(1 + et + et^2))/7}, 
     {(-I + Sqrt[3] + 2*Sqrt[3]*et^2 + (-3*I + Sqrt[3])*et^3 + 
        (-3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, 
      (I + Sqrt[3] + 2*Sqrt[3]*et^2 + (3*I + Sqrt[3])*et^3 + 
        (3*I + Sqrt[3])*et^4 + 2*Sqrt[3]*et^5)/14, -(et*(-2 + et^3 + et^4))/
       7, -(-1 + et^2)^2/7, -((-1 + et)^2*et^3)/7, -((-1 + et)^2*et*(1 + et))/
       7, (2 + et + et^2 + 2*et^3 + et^5)/7, 
      (et*(1 + 2*et + et^2 + 2*et^3 + et^4))/7}}
 
b7ToNum = {b7 -> (-1 + I*Sqrt[7])/(2*Sqrt[2]), 
     d7 -> (-1 - I*Sqrt[7])/(2*Sqrt[2])}
 
b7ToEt = {b7 -> (et + et^2 + et^4)/Sqrt[2], 
     d7 -> (et^3 + et^5 + et^6)/Sqrt[2]}
 
et4ToB7 = {et^4 -> Sqrt[2]*b7 - et - et^2}
 
et2Num = {et -> E^(((2*I)/7)*Pi)}
