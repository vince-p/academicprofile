@for /R %%I in (*.png) do "D:\sync\OneDrive - Macquarie University (1)\apps\IrfanView\i_view32.exe" "%%I" /resize=(200,0) /aspectratio /convert="%%I"

@for /R %%I in (*.jpg) do "D:\sync\OneDrive - Macquarie University (1)\apps\IrfanView\i_view32.exe" "%%I" /resize=(200,0) /aspectratio /convert="%%I"