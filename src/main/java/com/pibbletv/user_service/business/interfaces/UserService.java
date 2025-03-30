package com.pibbletv.user_service.business.interfaces;
import com.pibbletv.user_service.domain.User;

public interface UserService {

void saveUser(String userId, String username);

User getUser(String userId);

}
