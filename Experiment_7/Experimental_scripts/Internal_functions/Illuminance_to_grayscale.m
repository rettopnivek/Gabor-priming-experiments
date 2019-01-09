function [ G ] = Illuminance_to_grayscale( I, A, gamma_coef )
% Purpose:
%   Calculates the grayscale value(s) given corresponding 
%   illuminance value(s) based on a power model.
% Arguments:
%   I          - A vector of illuminance values
%   A          - Scale parameter (Scales magnitude of illuminace values)
%   gamma_coef - Rate parameter (Rate of change of illuminance based on
%                grayscale)
% Returns:
%   The corresponding grayscale value(s).

G = (I./A).^(1./gamma_coef);

end

