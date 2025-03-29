package com.pibbletv.user_service.business.interfaces;
import com.pibbletv.user_service.domain.User;

public interface UserService {

void saveUser(String token);

User getUser(String token);

}
