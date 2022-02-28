\ignore{
\begin{code}
module GraduationThesis.Lhs.Introduction where
import GraduationThesis.Lhs.Implementation
\end{code}
}

As human tecnologies advances, we are interested in convering a broader spectrum of real world phenomena, including physical behaviours. Current computers' discrete nature adds a limitation in modeling the continuous aspect of such phenomena; thus becoming a still-unsolved challenge. Cyber physical systems (CPS) --- the integration of computers and physical processes~\cite{LeeModeling} --- tackle this problem by attempting to model the continuous meaning of \textit{time}, i.e., treating time as a measurement of \textit{correctness}, not \textit{performance}~\cite{LeeModeling}. Additionally, many processes may occur in parallel, requiring precise and sensitive management of time by the model.

Examples of such models are older than the digital computer; analog computers were used to model the fire system of battleships and core functionalities of aviation systems. The mechanical metrics involved in these problems are inherintly continuous, such as space, speed and area, e.g., the shoot speed and range are crucial in fire systems, and surfaces of control are essential to model planes' flaps. The main goal of such models was, and still is, to abstract away the continuous facet of the problem to the computer. In this manner, the human in the loop aspect only matters when interfacing with the computer, with all the heavy-lifting being done by shafters and gears in analog machines, and by software after the digital era.

To cope with the incompatibility of these sets of abstractions --- the discreteness of digital computers with the continuous nature of physical phenomena --- is the main goal of the present work.

\ignore{
\begin{code}
lorenzSpecs = Specs { startTime = 0,
                      stopTime  = 10,
                      dt        = 1,
                      method    = Euler
                    }

sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

lorenzModel :: Model [Double]
lorenzModel =
  do integX <- newInteg 1.0
     integY <- newInteg 1.0
     integZ <- newInteg 1.0
     let x = integValue integX
         y = integValue integY
         z = integValue integZ
     integDiff integX (sigma*(y-x))
     integDiff integY (x*(rho-z)-y)
     integDiff integZ (x*y-beta*z)
     return $ sequence [x,y,z]

executeLorenz =
  do ans <- runDynamicsFinal lorenzModel lorenzSpecs
     print ans
\end{code}
}
