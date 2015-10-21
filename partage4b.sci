function [xycode]=partage4b(x,y,xmin,ymin,xmax,ymax)
    xm=(xmin+xmax)/2;
    ym=(ymin+ymax)/2;
    x0=x(x<xm);
    x1=x(~(x<xm));
    y0=y(x<xm);
    y1=y(~(x<xm));
    x00=x0(y0<ym);
    x01=x0(~(y0<ym));
    x10=x1(y1<ym);
    x11=x1(~(y1<ym));
    y00=y0(y0<ym);
    y01=y0(~(y0<ym));
    y10=y1(y1<ym);
    y11=y1(~(y1<ym));
    code00=0*ones(x00);
    code01=1*ones(x01);
    code10=2*ones(x10);
    code11=3*ones(x11);
    xycode=[x00 y00 code00;
            x01 y01 code01;
            x10 y10 code10;
            x11 y11 code11];
endfunction
