# valuac_deriv
Valuación de derivados financieros y metodologías de simulación. 

valuac_deriv tiene como función recopilar los códigos utilizados para el segundo proyecto de finanzas cuantitativas del departamento de Matmáticas y Físca del Instituto Tecnológico y de Estudios Superiores de Occidente (ITESO). 

En este proyecto se decide participar en el primer concurso de Ingeniería Financiera (CIFI), abordando el tema de coberturas. De forma similar se tiene especial interés en aplicar la metodología MCMC (Markov Chain MonteCarlo) para simulación de procesos financieros. 

Para más información sobre el concurso CIFI, ver [aquí](https://drive.google.com/folder/d/0B1mUVJC7-ZYrfjJ0UTZtaVFuQmdicmpWaHdhXzlWVktrWXRoY3cyWDhTRXQ1Y2oyaTB4S0U/edit)

Información y literatura relevante sobre la teoría desarrollada en este proyecto puede ser vista en nuestro [GoogleDrive](https://drive.google.com/open?id=0B1mUVJC7-ZYrS0hEdzktX3BzUG8).

Para la redacción de aspectos teóricos, en este [sharelatex](https://www.sharelatex.com/project/56426fe6c8a1b56a3a0a1dd2).

### Integrantes del equipo

A continuación se identifica a los autores de este proyecto.

* [Luis](lcvirgen@gmail.com) Anotnio Cortez Virgen
* [Daniela](dannyguerralcala@gmail.com) Guerra Alcalá
* [Rodrigo](rohdzmota@gmail.com) Hernández Mota
* [Raúl](raul7romero@gmail.com) Romero Barragán

*Profesor*: **Juan Diego Sánchez Torres**

## Generalidades del proyecto

Se tiene la serie de tiempo de la paridad USD - MXN. Se pretende seguir el siguiente procedimiento y metodología.

1. Proponer modelos que pudieran explicar el proceso estocástico del tipo de cambio
    1. [agregar modelos]
2. Estimas los parámetros de tales modelos con los datos empíricos y mediante el método de máxima verosimilitud.
3. Evaluar los modelos y determinar cual se ajusta mejor a los datos.
4. Poryectar o forecast la realización de la variable aleatoria a tiempo futuro.
5. Evaluar la efectividad de ciertas estrategias y coberturas financieras (opciones, forwards y bonos).
6. Presentar resultados y proponer estrategia según las necesidades de la empresa. 

## Generalidades github
Esta sección tiene como propósito orientar a aquellos que no conocen sobre github. Se recomienda utilizar los siguientes comandos en la pantalla de comando GITBASH.

Comandos relevantes:

    cd "carpeta"
    cd ..
    git clone
    git config {user.name, user.email} "desc"

A continuación, el procedimiento sugerido:

    0 git remote add origin URL hace referencia a una carpeta en linea
    1 git pull origin master
    2 hacer cambios pertinentes
    3 git add
    4 git commit -m "desc"
    5 git push origin master

