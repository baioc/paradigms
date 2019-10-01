\ https://www.inf.ufsc.br/~alvaro.junio/notas-seccom-2019.pdf

Maximize
  35 [ x_S1 + x_S2 + x_S3 + x_S4 ] + 28 [ x_Z1 + x_Z2 + x_Z3 + x_Z4 ] + 22 [ x_A1 + x_A2 + x_A3 + x_A4 ] -19 [ x_S1 + x_Z1 + x_A1 ] -24 [ x_S2 + x_Z2 + x_A2 ] -20 [ x_S3 + x_Z3 + x_A3 ] -27 [ x_S4 + x_Z4 + x_A4 ]

Subject To
  x_S1 + x_Z1 + x_A1 <= 3500
  x_S2 + x_Z2 + x_A2 <= 2200
  x_S3 + x_Z3 + x_A3 <= 4200
  x_S4 + x_Z4 + x_A4 <= 1800

  x_S1 <= 0.3 * [ x_S1 + x_S2 + x_S3 + x_S4 ]
  x_S2 >= 0.4 * [ x_S1 + x_S2 + x_S3 + x_S4 ]
  x_S3 <= 0.5 * [ x_S1 + x_S2 + x_S3 + x_S4 ]

  x_Z1 <= 0.3 * [ x_Z1 + x_Z2 + x_Z3 + x_Z4 ]
  x_Z2 >= 0.1 * [ x_Z1 + x_Z2 + x_Z3 + x_Z4 ]

  x_A1 <= 0.7 * [ x_A1 + x_A2 + x_A3 + x_A4 ]

Bounds
  0 <= x_S1
  0 <= x_S2
  0 <= x_S3
  0 <= x_S4
  0 <= x_Z1
  0 <= x_Z2
  0 <= x_Z3
  0 <= x_Z4
  0 <= x_A1
  0 <= x_A2
  0 <= x_A3
  0 <= x_A4

Generals
  x_S1 x_S2 x_S3 x_S4
  x_Z1 x_Z2 x_Z3 x_Z4
  x_A1 x_A2 x_A3 x_A4

End
