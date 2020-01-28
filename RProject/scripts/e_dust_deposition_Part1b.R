
# Direcao do Vento http://tornado.sfsu.edu/geosciences/classes/m430/Wind/WindDirection.html
# https://www.wxforum.net/index.php?topic=8660.0
C_zonal_U      = round( -dustDep_temp$vento_vel * sind(dustDep_temp$vento_dir) , 6)
C_meridional_V = round( -dustDep_temp$vento_vel * cosd(dustDep_temp$vento_dir) , 6)

# Direcao resultante (DVr, °):
DVr1 = round( atan  (sum(C_zonal_U) / sum(C_meridional_V)) , 6 ) 
DVr2 = round( atan2 (sum(C_zonal_U) , sum(C_meridional_V)) , 6 ) 


# Devido a descontinuidade de DVr no ângulo de 360°, aplica-se a análise o 
# índice de direção do vento (IDV). O IDV é uma transformação aritmética que
# expressa a direção do vento resultante como função contínua (Chaloulakou et al., 2003a)
# DVr é a direção do vento (rad), com o norte correspondendo a 0

fi = 0
IDV1 = round(1 + sin(DVr1 - fi ), 6) #-(-0.03313127))   )
IDV2 = round(1 + sin(DVr2 - fi ), 6)

aux_DirVento <- rbind(aux_DirVento, list(DVr1, IDV1, DVr2, IDV2), deparse.level = 1)

#
# ao utilizar a funcao atan eh encontrado o problema de descontinuidade 
# nos intervalos de [0° - 90°, 180° - 270°] quando eh utilizada a funcao
# atan2, o proprio software já corrige o problema de descontinuidade 
# dividindo os termos por -pi
#
# a funcao atan encontra um problema com DVr < 0, mostrando os angulos 
# complementares (quadrantes opostos) a funcao atan2 não apresenta
#  esse problema, e os valores estão nos quadrantes corretos
# 
# https://en.wikipedia.org/wiki/Atan2
# 